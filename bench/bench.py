#!/usr/bin/env python3
"""
Benchmark harness: naive-modcheck-coalg vs COOL-MC (cool-coalg).

For each (logic, generator, size): emits a shared parity-game-enriched model
file plus two target-specific formula files, runs both tools via hyperfine,
captures a one-off truth value from each (to ensure correctness), 
and appends long-form rows to a CSV for downstream plotting.

Timing methodology mirrors COOL-MC's own benchmark harness
(benchmarks/modcheck/parity-mod/bench.py):
- hyperfine with one warmup + N runs
- exponential size growth
- timeout that halts the sweep for the (tool, logic, generator) triple 
  once the wall clock timeouts.
"""
from __future__ import annotations

import argparse
import csv
import re
import shlex
import shutil
import subprocess
import sys
import tempfile
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, Iterator

ROOT = Path(__file__).resolve().parents[1]
GENERATED_DIR = ROOT / "generated"
MODELS_DIR = GENERATED_DIR / "models"
FORMULAS_DIR = GENERATED_DIR / "formulas"
RESULTS_DIR = GENERATED_DIR / "bench_results"
PLOTS_DIR = GENERATED_DIR / "plots"
DEFAULT_CSV = RESULTS_DIR / "results.csv"
DEFAULT_COOL = ROOT / "bench" / "cool-mc" / "cool-coalg"

GENERATORS = (
    "cliquegame",
    "laddergame",
    "jurdzinskigame",
    "towersofhanoi",
    "langincl",
)
LOGICS = ("relational", "probabilistic")
TOOLS = ("naive", "cool", "pgsolver")

MODEL_EXT = {"relational": ".model", "probabilistic": ".prob.model"}
COOL_FUNCTOR = {"relational": "K", "probabilistic": "PML_react"}
# cool-coalg can back modcheck with multiple parity-game solvers. We compare its
# own "cool" solver against the pgsolver library. Both share one formula file.
COOL_GAMESOLVER = {"cool": "cool", "pgsolver": "pgsolver"}
FORMULA_TARGET = {"naive": "naive", "cool": "cool", "pgsolver": "cool"}

CSV_FIELDS = [
    "tool",
    "logic",
    "generator",
    "size",
    "mean",
    "stddev",
    "median",
    "min",
    "max",
    "result",
    "timed_out",
]


def log(msg: str) -> None:
    print(msg, flush=True)


def run_cmd(cmd, **kwargs) -> subprocess.CompletedProcess:
    """Run a subprocess with stderr captured (subprocess.run wrapper). 
    Raises when exit code is non-zero."""
    return subprocess.run(
        cmd,
        cwd=kwargs.pop("cwd", ROOT),
        check=kwargs.pop("check", True),
        stdout=kwargs.pop("stdout", subprocess.PIPE),
        stderr=kwargs.pop("stderr", subprocess.PIPE),
        text=kwargs.pop("text", True),
        **kwargs,
    )


def ensure_model_file(logic: str, generator: str, size: int) -> Path:
    """Ensure the model file for the given (logic, generator, size) exists.
    Otherwise, run model_gen.exe to produce it and return the path."""
    out_dir = MODELS_DIR / logic
    out_dir.mkdir(parents=True, exist_ok=True)
    path = out_dir / f"{generator}_{size}{MODEL_EXT[logic]}"
    if path.exists():
        return path
    fmt = "rel-model" if logic == "relational" else "prob-model"
    run_cmd(
        [
            "dune",
            "exec",
            "bench/model_gen.exe",
            "--",
            generator,
            str(size),
            "--format",
            fmt,
            "-o",
            str(out_dir),
        ]
    )
    if not path.exists():
        raise RuntimeError(f"model_gen did not produce {path}")
    return path


def ensure_formula_file(logic: str, tool: str, max_prio: int) -> Path:
    """Ensure the formula file for the given (logic, tool, max_prio) exists.
    Otherwise, run formula_gen.exe to produce it and return the path."""
    out_dir = FORMULAS_DIR / logic / tool
    out_dir.mkdir(parents=True, exist_ok=True)
    logic_tag = "rel" if logic == "relational" else "prob"
    path = out_dir / f"formula_{logic_tag}_{tool}_{max_prio}.mcf"
    if path.exists():
        return path
    run_cmd(
        [
            "dune",
            "exec",
            "bench/formula_gen.exe",
            "--",
            str(max_prio),
            "--logic",
            logic,
            "--target",
            tool,
            "-o",
            str(out_dir),
        ]
    )
    if not path.exists():
        raise RuntimeError(f"formula_gen did not produce {path}")
    return path


def max_priority_for(generator: str, size: int) -> int:
    """Max priority index for a generator's game of the given size.

    Must match model_gen.ml's default_args so the formula's priority range
    lines up with the game's. The formula is disjunction over priorities
    0..d where d is the value returned here.
    """
    if generator == "cliquegame":
        # default_args passes n+1, giving priorities 0..n.
        return size
    if generator == "jurdzinskigame":
        # default_args passes (h=n, w=n); max prio = 2n+1.
        return 2 * size + 1
    if generator in ("laddergame", "towersofhanoi"):
        return 1
    if generator == "langincl":
        return 2
    raise ValueError(f"unknown generator: {generator}")


@dataclass
class Invocation:
    """Instructions for running a single (tool, logic, generator, size) cell."""

    check_cmd: str
    result_parser: "type[ResultParser]"


class ResultParser:
    """Parse stdout of one check run into a normalized truth label."""

    label = "?"

    @classmethod
    def parse(cls, stdout: str) -> str:
        """Return the parsed truth label from the stdout (true/false/?). 
        Override in subclasses."""
        return "?"


class NaiveResultParser(ResultParser):
    label = "naive"

    RESULT_RE = re.compile(r"\(result\s+(true|false)\)")

    @classmethod
    def parse(cls, stdout: str) -> str:
        m = cls.RESULT_RE.search(stdout)
        return m.group(1) if m else "?"


class CoolResultParser(ResultParser):
    label = "cool"

    @classmethod
    def parse(cls, stdout: str) -> str:
        if "unsatisfied" in stdout:
            return "false"
        if "satisfied" in stdout:
            return "true"
        return "?"


def build_invocation(
    tool: str,
    logic: str,
    model_path: Path,
    formula_path: Path,
    timeout_seconds: int,
    cool_bin: Path,
) -> Invocation:
    """Build the Invocation (instructions) for the given (tool, logic) pair on the given model/formula."""
    point = "s0"
    if tool == "naive":
        cmd = (
            f"timeout {timeout_seconds}s dune exec naive-modcheck-coalg -- "
            f"--logic {logic} "
            f"--model-file {shlex.quote(str(model_path))} "
            f"--formula-file {shlex.quote(str(formula_path))} "
            f"--point {point}"
        )
        return Invocation(cmd, NaiveResultParser)
    if tool in COOL_GAMESOLVER:
        functor = COOL_FUNCTOR[logic]
        solver = COOL_GAMESOLVER[tool]
        cmd = (
            f"cat {shlex.quote(str(formula_path))} | "
            f"timeout {timeout_seconds}s {shlex.quote(str(cool_bin))} "
            f"modcheck {functor} "
            f"-m {shlex.quote(str(model_path))} "
            f"-p {point} "
            f"--gameSolver {solver}"
        )
        return Invocation(cmd, CoolResultParser)
    raise ValueError(f"unknown tool: {tool}")


def get_timeout_csv_row(tool: str, logic: str, generator: str, size: int) -> dict:
    """Get a CSV row dict for a timed-out cell."""
    return {
        "tool": tool,
        "logic": logic,
        "generator": generator,
        "size": size,
        "mean": "",
        "stddev": "",
        "median": "",
        "min": "",
        "max": "",
        "result": "",
        "timed_out": "true",
    }


def hyperfine_row_to_csv_row(
    tool: str,
    logic: str,
    generator: str,
    size: int,
    hf_row: dict,
    result: str,
) -> dict:
    """Convert a hyperfine row and the captured result into a long-form CSV row dict."""
    return {
        "tool": tool,
        "logic": logic,
        "generator": generator,
        "size": size,
        "mean": hf_row.get("mean", ""),
        "stddev": hf_row.get("stddev", ""),
        "median": hf_row.get("median", ""),
        "min": hf_row.get("min", ""),
        "max": hf_row.get("max", ""),
        "result": result,
        "timed_out": "false",
    }


def capture_result(check_cmd: str, parser: type[ResultParser]) -> str:
    """Run the check command once (outside hyperfine) to capture the truth value.
    This is to ensure correctness of results."""
    try:
        proc = subprocess.run(
            check_cmd,
            shell=True,
            cwd=ROOT,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            timeout=None,
        )
    except subprocess.TimeoutExpired:
        return "?"
    if proc.returncode == 124:
        return "?"
    return parser.parse(proc.stdout + proc.stderr)


def run_hyperfine(
    check_cmd: str, runs: int, warmup: int
) -> tuple[dict | None, bool]:
    """Returns (parsed hyperfine row, timed_out?)."""
    with tempfile.NamedTemporaryFile(
        mode="w+", suffix=".csv", delete=False
    ) as tf:
        csv_path = Path(tf.name)
    try:
        cmd = [
            "hyperfine",
            "--runs",
            str(runs),
            "--warmup",
            str(warmup),
            "--export-csv",
            str(csv_path),
            check_cmd,
        ]
        proc = subprocess.run(
            cmd,
            cwd=ROOT,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
        )
        if proc.returncode != 0:
            stderr = (proc.stderr or "").lower()
            timed_out = (
                "exit code: 124" in stderr
                or "exit code 124" in stderr
                or "timed out" in stderr
                or "timeout" in stderr
            )
            return None, timed_out
        with csv_path.open() as fh:
            reader = csv.DictReader(fh)
            rows = list(reader)
        return (rows[0] if rows else None), False
    finally:
        csv_path.unlink(missing_ok=True)


def benchmark_sizes(start: int, max_size: int) -> Iterator[int]:
    """Generate benchmark sizes from start to max_size, exponential growth."""
    size = start
    cnt = 0
    while size <= max_size:
        yield size
        cnt += 1
        size += 10 ** (cnt // 10)


def append_row(writer: csv.DictWriter, handle, row: dict) -> None:
    writer.writerow(row)
    handle.flush()


def run_cell(
    writer: csv.DictWriter,
    handle,
    tool: str,
    logic: str,
    generator: str,
    size: int,
    model_path: Path,
    formula_path: Path,
    runs: int,
    warmup: int,
    timeout_seconds: int,
    cool_bin: Path,
) -> bool:
    """Run one (tool, logic, generator, size) cell. Returns False on timeout."""
    inv = build_invocation(
        tool, logic, model_path, formula_path, timeout_seconds, cool_bin
    )

    # Hyperfine
    log(f"  [{tool}/{logic}/{generator}/{size}] hyperfine (timeout {timeout_seconds}s)...")
    hf_row, timed_out = run_hyperfine(inv.check_cmd, runs, warmup)
    if timed_out or hf_row is None:
        log(f"    timed out — halting {tool}/{logic}/{generator} chain")
        append_row(writer, handle, get_timeout_csv_row(
            tool, logic, generator, size))
        return False
    
    # Capture result
    log(f"  [{tool}/{logic}/{generator}/{size}] capture truth value...")
    result = capture_result(inv.check_cmd, inv.result_parser)
    row = hyperfine_row_to_csv_row(
        tool, logic, generator, size, hf_row, result
    )
    append_row(writer, handle, row)
    log(f"    mean={hf_row.get('mean')} result={result}")
    return True


def main_loop(args: argparse.Namespace) -> Path:
    output_csv = Path(args.output_csv)
    output_csv.parent.mkdir(parents=True, exist_ok=True)
    write_header = not output_csv.exists() or args.fresh
    mode = "w" if args.fresh else "a"

    with output_csv.open(mode, newline="") as fh:
        writer = csv.DictWriter(fh, fieldnames=CSV_FIELDS)
        if write_header:
            writer.writeheader()
            fh.flush()

        for logic in args.logics:
            for generator in args.generators:
                # One (tool, logic, generator) chain should halt independently
                # once it times out.
                active = {tool: True for tool in args.tools}
                for size in benchmark_sizes(args.start, args.max_size):
                    if not any(active.values()):
                        break
                    log(f"[{logic}/{generator}/size={size}] generating inputs")
                    max_prio = max_priority_for(generator, size)
                    try:
                        model_path = ensure_model_file(logic, generator, size)
                    except subprocess.CalledProcessError as exc:
                        log(
                            f"  model_gen failed for {generator}/{size}: "
                            f"{exc.stderr}"
                        )
                        break
                    formulas = {}
                    for tool in args.tools:
                        if not active[tool]:
                            continue
                        formulas[tool] = ensure_formula_file(
                            logic, FORMULA_TARGET[tool], max_prio)
                    for tool in args.tools:
                        if not active[tool]:
                            continue
                        if not run_cell(
                            writer,
                            fh,
                            tool,
                            logic,
                            generator,
                            size,
                            model_path,
                            formulas[tool],
                            args.runs,
                            args.warmup,
                            args.timeout,
                            args.cool_bin,
                        ):
                            active[tool] = False
    return output_csv


def parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser(
        description=(
            "Comprehensive benchmark for naive-modcheck-coalg vs COOL over "
            "parity-game-enriched relational and probabilistic mu-calculus "
            "model-checking instances."
        )
    )
    p.add_argument("--start", type=int, default=2)
    p.add_argument("--max-size", type=int, default=1024)
    p.add_argument("--runs", type=int, default=3)
    p.add_argument("--warmup", type=int, default=1)
    p.add_argument("--timeout", type=int, default=60)
    p.add_argument(
        "--logics",
        default=",".join(LOGICS),
        help="Comma-separated subset of {relational, probabilistic}.",
    )
    p.add_argument(
        "--generators",
        default=",".join(GENERATORS),
        help="Comma-separated subset of the supported generators.",
    )
    p.add_argument(
        "--tools",
        default=",".join(TOOLS),
        help="Comma-separated subset of {naive, cool, pgsolver}.",
    )
    p.add_argument(
        "--output-csv",
        type=Path,
        default=DEFAULT_CSV,
        help="Long-form results CSV (appended to by default).",
    )
    p.add_argument(
        "--cool-bin",
        type=Path,
        default=DEFAULT_COOL,
        help="Path to the cool-coalg binary.",
    )
    p.add_argument(
        "--fresh",
        action="store_true",
        help="Overwrite the CSV instead of appending.",
    )
    p.add_argument(
        "--plots",
        action="store_true",
        help="Run bench/plot.py after the sweep finishes.",
    )
    a = p.parse_args()
    a.logics = _validate_subset("logic", a.logics.split(","), LOGICS)
    a.generators = _validate_subset(
        "generator", a.generators.split(","), GENERATORS
    )
    a.tools = _validate_subset("tool", a.tools.split(","), TOOLS)
    if a.start <= 0:
        p.error("--start must be positive")
    if a.max_size < a.start:
        p.error("--max-size must be >= --start")
    if a.runs <= 0 or a.warmup < 0 or a.timeout <= 0:
        p.error("--runs/--warmup/--timeout must be non-negative / positive")
    if (set(a.tools) & set(COOL_GAMESOLVER)) and not a.cool_bin.exists():
        p.error(f"cool-coalg binary not found at {a.cool_bin}")
    return a


def _validate_subset(
    label: str, given: Iterable[str], allowed: Iterable[str]
) -> list[str]:
    allowed_set = set(allowed)
    picked = []
    for g in given:
        g = g.strip()
        if not g:
            continue
        if g not in allowed_set:
            raise SystemExit(
                f"Unknown {label}: {g!r}. Choose from {sorted(allowed_set)}."
            )
        if g not in picked:
            picked.append(g)
    if not picked:
        raise SystemExit(f"No {label}s selected.")
    return picked


def main() -> None:
    args = parse_args()
    if shutil.which("hyperfine") is None:
        sys.exit("hyperfine not on PATH; install it before running.")
    MODELS_DIR.mkdir(parents=True, exist_ok=True)
    FORMULAS_DIR.mkdir(parents=True, exist_ok=True)
    RESULTS_DIR.mkdir(parents=True, exist_ok=True)
    csv_path = main_loop(args)
    log(f"done — results at {csv_path}")
    if args.plots:
        plot_py = ROOT / "bench" / "plot.py"
        if plot_py.exists():
            run_cmd(
                ["python3", str(plot_py), str(csv_path)],
                stdout=None,
                stderr=None,
                check=False,
            )


if __name__ == "__main__":
    main()
