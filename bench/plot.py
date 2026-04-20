#!/usr/bin/env python3
"""
Plot benchmark results from bench.py's long-form CSV.

For each (logic, generator):
  - one log-log figure with one line per tool (mean +/- stddev),
  - markers at the largest non-timed-out size,
  - a vertical "timeout wall" at the first timed-out size, if any.

Plus one summary speedup figure per logic: geometric-mean of cool/naive means
across sizes, faceted by generator.

Also writes a short console report of:
  - (tool, logic, generator) chains that hit the timeout wall,
  - (logic, generator, size) rows where the two tools disagree on truth value.
"""
from __future__ import annotations

import argparse
import sys
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[1]
DEFAULT_CSV = ROOT / "generated" / "bench_results" / "results.csv"
PLOTS_DIR = ROOT / "generated" / "plots"

TOOL_STYLE = {
    "naive": {"color": "#1f77b4", "marker": "o", "label": "naive-modcheck-coalg"},
    "cool": {"color": "#d62728", "marker": "s", "label": "cool-coalg (--gameSolver cool)"},
    "pgsolver": {"color": "#2ca02c", "marker": "^", "label": "cool-coalg (--gameSolver pgsolver)"},
}
BASELINE_TOOL = "naive"


def load(csv_path: Path) -> pd.DataFrame:
    df = pd.read_csv(csv_path)
    df.columns = df.columns.str.strip()
    for col in ("mean", "stddev", "median", "min", "max", "size"):
        if col in df.columns:
            df[col] = pd.to_numeric(df[col], errors="coerce")
    df["timed_out"] = df["timed_out"].astype(str).str.lower() == "true"
    return df


def plot_cell(df: pd.DataFrame, logic: str, generator: str, out_dir: Path) -> None:
    sub = df[(df.logic == logic) & (df.generator == generator)]
    if sub.empty:
        return
    out_dir.mkdir(parents=True, exist_ok=True)
    fig, ax = plt.subplots(figsize=(7, 4.5))
    timeout_sizes = []
    for tool, style in TOOL_STYLE.items():
        tsub = sub[sub.tool == tool].sort_values("size")
        if tsub.empty:
            continue
        ok = tsub[~tsub.timed_out]
        if not ok.empty:
            ax.errorbar(
                ok["size"],
                ok["mean"],
                yerr=ok["stddev"].fillna(0),
                marker=style["marker"],
                color=style["color"],
                label=style["label"],
                linewidth=1.4,
                capsize=3,
            )
        to = tsub[tsub.timed_out]
        if not to.empty:
            timeout_sizes.append((tool, int(to["size"].min())))
    for tool, size in timeout_sizes:
        ax.axvline(
            size,
            color=TOOL_STYLE[tool]["color"],
            linestyle=":",
            alpha=0.5,
            label=f"{TOOL_STYLE[tool]['label']} timeout ≥ {size}",
        )
    ax.set_xscale("log")
    ax.set_yscale("log")
    ax.set_xlabel("size (parity-game parameter)")
    ax.set_ylabel("wall time (s)")
    ax.set_title(f"{logic} / {generator}")
    ax.grid(which="both", alpha=0.3)
    ax.legend(fontsize=8)
    fig.tight_layout()
    fig.savefig(out_dir / f"{generator}.png", dpi=130)
    plt.close(fig)


def plot_speedup(df: pd.DataFrame, logic: str, out_dir: Path) -> None:
    sub = df[(df.logic == logic) & (~df.timed_out)]
    if sub.empty:
        return
    pivot = sub.pivot_table(
        index=["generator", "size"],
        columns="tool",
        values="mean",
        aggfunc="mean",
    )
    if BASELINE_TOOL not in pivot.columns:
        return
    others = [t for t in pivot.columns if t != BASELINE_TOOL and t in TOOL_STYLE]
    if not others:
        return
    generators = sorted(df[df.logic == logic].generator.unique())
    cols = min(2, len(generators))
    rows = (len(generators) + cols - 1) // cols
    fig, axes = plt.subplots(
        rows, cols, figsize=(6 * cols, 3.5 * rows), squeeze=False
    )
    for ax, gen in zip(axes.flat, generators):
        for tool in others:
            gsub = pivot.loc[(gen,)] if (gen,) in pivot.index else None
            try:
                gsub = pivot.xs(gen, level="generator")
            except KeyError:
                continue
            gsub = gsub.dropna(subset=[BASELINE_TOOL, tool])
            if gsub.empty:
                continue
            ratio = gsub[tool] / gsub[BASELINE_TOOL]
            ax.plot(
                gsub.index,
                ratio,
                marker=TOOL_STYLE[tool]["marker"],
                color=TOOL_STYLE[tool]["color"],
                label=TOOL_STYLE[tool]["label"],
            )
        ax.axhline(1.0, color="black", linestyle="--", alpha=0.4)
        ax.set_xscale("log")
        ax.set_yscale("log")
        ax.set_xlabel("size")
        ax.set_ylabel(f"mean / {BASELINE_TOOL} (ratio)")
        ax.set_title(gen)
        ax.grid(which="both", alpha=0.3)
        ax.legend(fontsize=8)
    for ax in axes.flat[len(generators) :]:
        ax.axis("off")
    fig.suptitle(
        f"{logic}: timing ratio vs {BASELINE_TOOL} (>1 ⇒ {BASELINE_TOOL} faster)"
    )
    fig.tight_layout()
    fig.savefig(out_dir / "_speedup.png", dpi=130)
    plt.close(fig)


def report_console(df: pd.DataFrame) -> None:
    to = df[df.timed_out]
    if not to.empty:
        print("\n[timeouts] first size at which each chain halted:")
        halted = (
            to.groupby(["tool", "logic", "generator"])["size"].min().reset_index()
        )
        for _, row in halted.iterrows():
            print(
                f"  {row.tool:>6} / {row.logic:<13} / {row.generator:<14} "
                f"→ size >= {int(row['size'])}"
            )
    good = df[(~df.timed_out) & (df.result.isin(["true", "false"]))]
    if good.empty:
        return
    pivot = good.pivot_table(
        index=["logic", "generator", "size"],
        columns="tool",
        values="result",
        aggfunc="first",
    )
    tools_present = [t for t in TOOL_STYLE if t in pivot.columns]
    if len(tools_present) < 2:
        return
    pivot = pivot[tools_present].dropna(how="any")
    mismatches = pivot[pivot.nunique(axis=1) > 1]
    if mismatches.empty:
        print("\n[correctness] no (logic, generator, size) mismatches across tools.")
        return
    print("\n[correctness] truth-value mismatches across tools:")
    for idx, row in mismatches.iterrows():
        logic, gen, size = idx
        pieces = " ".join(f"{t}={row[t]}" for t in tools_present)
        print(f"  {logic:<13} / {gen:<14} / size={int(size):<5} {pieces}")


def main() -> None:
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument(
        "csv", nargs="?", default=str(DEFAULT_CSV), help="results CSV path"
    )
    p.add_argument("--out", default=str(PLOTS_DIR), help="plots output dir")
    args = p.parse_args()
    csv_path = Path(args.csv)
    out_root = Path(args.out)
    if not csv_path.exists():
        sys.exit(f"no results CSV at {csv_path}")
    df = load(csv_path)
    if df.empty:
        sys.exit("results CSV is empty")
    for logic in sorted(df.logic.unique()):
        ldir = out_root / logic
        for generator in sorted(df.generator.unique()):
            plot_cell(df, logic, generator, ldir)
        plot_speedup(df, logic, ldir)
    print(f"wrote plots under {out_root}")
    report_console(df)


if __name__ == "__main__":
    main()
