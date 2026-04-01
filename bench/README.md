# Benchmarks

This directory contains scripts/utilities for benchmark generation and execution.

## What `bench.py` does

For each game generator, the script iterates all benchmark sizes and for each size:

1. Generates a relational formula (if missing).
2. Generates that generator's relational model (if missing).
3. Runs the relational model checker via `hyperfine`.

Sizes increase each iteration using:

`size += (10 ** (iter_cnt // 10))`

## Prerequisites

- Python 3
- `dune`
- `hyperfine`
- `timeout` (from GNU coreutils)

From project root, make sure executables build:

```bash
dune build bench/model_gen.exe bench/formula_gen.exe naive-modcheck-coalg
```

## Usage

Run from repository root:

```bash
python3 bench/bench.py \
  --start 2 \
  --max-size 256 \
  --runs 3 \
  --timeout 30
```

### CLI options

- `--start`: smallest benchmark size (default: `2`)
- `--max-size`: largest benchmark size (default: `256`)
- `--runs`: hyperfine runs per size (default: `3`)
- `--timeout`: timeout in seconds for each benchmark run (default: `30`)

The script benchmarks all currently supported generators:

- `randomgame`
- `laddergame`
- `towersofhanoi`
- `langincl`

## Timeout behavior

Each benchmark command is wrapped as:

```bash
timeout 30s dune exec naive-modcheck-coalg -- ...
```

So each single run has a 30-second cap.

## Output layout

Generated artifacts are cached under `generated/`:

- Models: `generated/models/<generator>_<size>.model`
- Formulas: `generated/formulas/formula_<size>.mcf`
- Hyperfine JSON per generator/size: `generated/bench_results/<generator>_size_<size>.json`

If model/formula files already exist, they are reused.

## Notes

- The checker is executed with relational logic and fixed point `s0`:
  - `--logic relational`
  - `--point s0`
- Ensure generated models include state `s0` (the current `model_gen` format does).
