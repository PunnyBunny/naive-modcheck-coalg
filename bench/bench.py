#!/usr/bin/env python3

import argparse
import shlex
import subprocess
from pathlib import Path
from typing import Iterator


ROOT = Path(__file__).resolve().parents[1]
GENERATED_DIR = ROOT / "generated"
MODELS_DIR = GENERATED_DIR / "models"
FORMULAS_DIR = GENERATED_DIR / "formulas"
RESULTS_DIR = GENERATED_DIR / "bench_results"
GENERATORS = ("randomgame", "laddergame", "towersofhanoi", "langincl")


def run_cmd(
    cmd, *, cwd=ROOT, timeout=None, stdout=None
) -> None:
    subprocess.run(
        cmd,
        cwd=cwd,
        check=True,
        timeout=timeout,
        stdout=stdout,
        stderr=subprocess.PIPE,
        text=True,
    )


def ensure_model(
    generator: str, size: int, model_path: Path
) -> None:
    if model_path.exists():
        return
    model_path.parent.mkdir(parents=True, exist_ok=True)
    cmd = [
        "dune",
        "exec",
        "bench/model_gen.exe",
        "--",
        generator,
        str(size),
        "--format",
        "model",
        "-o",
        str(model_path.parent),
    ]
    run_cmd(cmd)


def ensure_formula(size: int, formula_path: Path) -> None:
    if formula_path.exists():
        return
    formula_path.parent.mkdir(parents=True, exist_ok=True)
    cmd = [
        "dune",
        "exec",
        "bench/formula_gen.exe",
        "--",
        str(size),
        "-o",
        str(formula_path.parent),
    ]
    run_cmd(cmd)


def run_benchmark(
    generator: str,
    size: int,
    model_path: Path,
    formula_path: Path,
    runs: int,
    timeout_seconds: int,
) -> None:
    print(f"Benchmarking size {size} with generator {generator}...")
    RESULTS_DIR.mkdir(parents=True, exist_ok=True)
    result_json = RESULTS_DIR / f"{generator}_size_{size}.json"

    check_cmd = (
        f"timeout {timeout_seconds}s dune exec naive-modcheck-coalg -- "
        "--logic relational "
        f"--model-file {shlex.quote(str(model_path))} "
        f"--formula-file {shlex.quote(str(formula_path))} "
        "--point s0"
    )

    hyperfine_cmd = [
        "hyperfine",
        "--runs",
        str(runs),
        "--warmup",
        "1",
        "--export-json",
        str(result_json),
        check_cmd,
    ]
    print(' '.join(hyperfine_cmd))
    try:
        run_cmd(hyperfine_cmd)
    except subprocess.CalledProcessError as exc:
        stderr = (exc.stderr or "").lower()
        timeout_failure = (
            "exit code: 124" in stderr
            or "exit code 124" in stderr
            or "timed out" in stderr
            or "timeout" in stderr
        )
        if timeout_failure:
            print(
                f"[generator={generator}] [size={size}] "
                "timed out; continuing"
            )
            return False
        raise
    return True


def benchmark_sizes(start: int, max_size: int) -> Iterator[int]:
    size = start
    iter_cnt = 0
    while size <= max_size:
        yield size
        iter_cnt += 1
        size += 10 ** (iter_cnt // 10)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Benchmark relational model checking over increasing "
            "sizes using hyperfine."
        )
    )
    parser.add_argument(
        "--start",
        type=int,
        default=2,
        help="Smallest size to benchmark (default: 2)",
    )
    parser.add_argument(
        "--max-size",
        type=int,
        default=256,
        help="Largest size to benchmark (default: 256)",
    )
    parser.add_argument(
        "--runs",
        type=int,
        default=3,
        help="Hyperfine runs per size (default: 3)",
    )
    parser.add_argument(
        "--timeout",
        type=int,
        default=30,
        help="Timeout in seconds for each benchmark run (default: 30)",
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()

    if args.start <= 0:
        raise ValueError("--start must be positive")
    if args.max_size < args.start:
        raise ValueError("--max-size must be >= --start")
    if args.runs <= 0:
        raise ValueError("--runs must be positive")
    if args.timeout <= 0:
        raise ValueError("--timeout must be positive")

    MODELS_DIR.mkdir(parents=True, exist_ok=True)
    FORMULAS_DIR.mkdir(parents=True, exist_ok=True)

    for generator in GENERATORS:
        print(f"[generator={generator}] start")
        for size in benchmark_sizes(args.start, args.max_size):
            formula_path = FORMULAS_DIR / f"formula_{size}.mcf"

            print(f"[generator={generator}] [size={size}] ensure formula")
            ensure_formula(size, formula_path)

            model_path = MODELS_DIR / f"{generator}_{size}.model"
            print(f"[generator={generator}] [size={size}] ensure model")
            ensure_model(generator, size, model_path)

            print(
                f"[generator={generator}] [size={size}] run hyperfine "
                f"(timeout {args.timeout}s)"
            )
            if not run_benchmark(
                generator,
                size,
                model_path,
                formula_path,
                args.runs,
                args.timeout,
            ):
                print(
                    f"[generator={generator}] [size={size}] "
                    "benchmark timed out; skipping larger sizes"
                )
                break


if __name__ == "__main__":
    main()
