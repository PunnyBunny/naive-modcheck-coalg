# Benchmarks: naive-modcheck-coalg vs COOL

Comprehensive benchmark harness for comparing `naive-modcheck-coalg` against
`cool-coalg` (from the COOL-MC paper, arxiv 2311.01315) over parity-game
derived relational and probabilistic mu-calculus model-checking instances.

## What the pipeline does

For each `(logic, generator, size)`:

1. `bench/model_gen.exe` builds a PGSolver parity game and enriches it per §4
   of the paper into a mu-calculus model file:
   - **Relational (K):** `[s_i: ({eloise|abelard, p_prio}, [a: {s_j, …}])]`
   - **Probabilistic (PML reactive):** `[s_i: ({…}, [a: [s_j: 1/n, …]])]`
   The model file is shared by both tools.
2. `bench/formula_gen.exe` emits two alternating-fixpoint formula files
   matching the game's priority count — one in the compositional syntax
   accepted by `naive-modcheck-coalg`, one in the ASCII syntax accepted by
   `cool-coalg`.
3. `bench/bench.py` invokes each tool through `hyperfine` (1 warmup, N runs),
   appends one long-form row per run to `generated/bench_results/results.csv`,
   and captures a one-off truth-value from each tool for cross-checking.
4. `bench/plot.py` (optional, triggered via `--plots`) draws one figure per
   `(logic, generator)` comparing the two tools, one `_speedup.png` summary
   per logic, and prints a console report of timeouts and truth-value
   mismatches.

Sizes grow exponentially: `size += 10 ** (iter // 10)`, matching COOL's own
benchmark harness.

## Prerequisites

- `dune`, `ocaml` (to build the OCaml generators and `naive-modcheck-coalg`).
- `hyperfine`, GNU `timeout` (from coreutils).
- Python ≥ 3.10 with `pandas`, `matplotlib`, `numpy`.
- `bench/cool-mc/cool-coalg` — the COOL binary. If you put it elsewhere, pass
  `--cool-bin /path/to/cool-coalg` to `bench.py`.

Build the OCaml pieces from the repository root:

```bash
dune build bench/model_gen.exe bench/formula_gen.exe naive-modcheck-coalg
```

## Running the benchmark

From the repository root:

```bash
python3 bench/bench.py --plots
```

The full default sweep (`--max-size 1024`, all 4 generators, both logics,
both tools) is long-running; for a quick smoke-test:

```bash
python3 bench/bench.py \
  --max-size 32 \
  --runs 2 --warmup 1 --timeout 30 \
  --logics relational,probabilistic \
  --generators laddergame,towersofhanoi \
  --tools naive,cool \
  --fresh --plots
```

### CLI options

| Flag | Default | Purpose |
|---|---|---|
| `--start` | `2` | Smallest size in the sweep. |
| `--max-size` | `1024` | Largest size. |
| `--runs` | `3` | Hyperfine measurement runs per cell. |
| `--warmup` | `1` | Hyperfine warmup runs per cell. |
| `--timeout` | `60` | Per-run wall-clock cap (seconds). |
| `--logics` | `relational,probabilistic` | Logics to sweep. |
| `--generators` | `cliquegame,laddergame,jurdzinskigame,towersofhanoi,langincl` | Parity-game generators. |
| `--tools` | `naive,cool,pgsolver` | Tools to compare. `cool` / `pgsolver` both invoke `cool-coalg`; they differ only in `--gameSolver`. |
| `--output-csv` | `generated/bench_results/results.csv` | Long-form results file. |
| `--cool-bin` | `bench/cool-mc/cool-coalg` | `cool-coalg` binary. |
| `--fresh` | *off* | Overwrite the CSV instead of appending. |
| `--plots` | *off* | Run `plot.py` at the end. |

Each `(tool, logic, generator)` chain halts independently at the first size
that exceeds `--timeout`, so the sweep never blocks on a single slow cell.

## Output layout

```
generated/
├── models/
│   ├── relational/<generator>_<size>.model
│   └── probabilistic/<generator>_<size>.prob.model
├── formulas/
│   ├── relational/
│   │   ├── naive/formula_rel_naive_<max_prio>.mcf
│   │   └── cool/formula_rel_cool_<max_prio>.mcf
│   └── probabilistic/
│       ├── naive/formula_prob_naive_<max_prio>.mcf
│       └── cool/formula_prob_cool_<max_prio>.mcf
├── bench_results/results.csv
└── plots/
    ├── relational/{<generator>.png, _speedup.png}
    └── probabilistic/{<generator>.png, _speedup.png}
```

## CSV schema

`generated/bench_results/results.csv` is long-form — one row per hyperfine
run. Columns:

| Column | Description |
|---|---|
| `tool` | `naive` \| `cool` |
| `logic` | `relational` \| `probabilistic` |
| `generator` | Parity-game generator name |
| `size` | Generator size parameter |
| `mean`/`stddev`/`median`/`min`/`max` | Hyperfine timings in seconds |
| `result` | Truth value parsed from stdout: `true` / `false` / `?` |
| `timed_out` | `true` if the run hit `--timeout` |

Regenerate plots only (without re-running the benchmark):

```bash
python3 bench/plot.py
```

## Notes

- Cached model and formula files under `generated/` are **reused across
  runs**; delete them or pass `--fresh` to the CSV to start from scratch.
- Generator argv conventions follow COOL's `benchmarks/modcheck/parity-mod`:
  `cliquegame n+1`, `jurdzinskigame n n`, `langincl n n`, `laddergame n`,
  `towersofhanoi n`. `cliquegame` and `jurdzinskigame` have `n`-scaling
  alternation depth and timeout early at larger sizes.
- PML truth values may differ between the two tools because each implements
  threshold modalities slightly differently. The benchmark reports these
  mismatches but still collects valid timing data for both tools.
- For the full default sweep on large sizes, run in the background:

  ```bash
  python3 bench/bench.py --plots > bench.log 2>&1 &
  ```
