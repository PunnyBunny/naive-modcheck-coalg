# naive-modcheck-coalg

Naive coalgebraic model checker for relational and probabilistic logics.

## Installation

### Prerequisites

- OCaml toolchain (`opam`)
- `dune`

The project depends on packages listed in `naive-modcheck-coalg.opam` (including `core`, `menhir`, `zarith`, `yojson`, `ounit2`).

### Setup (opam)

```bash
opam switch create . 5.2.0 --deps-only --with-test
eval $(opam env)
```

If you already have a switch, install deps only:

```bash
opam install . --deps-only --with-test
```

### Build

```bash
dune build
```

## Run

The CLI executable is `naive-modcheck-coalg`.

### Basic invocation

```bash
dune exec naive-modcheck-coalg -- \
  --logic relational \
  --model "[x : ({p1, p2}, [a : {x, y}]), y : ({p2}, [a : {x, y}])]" \
  --formula "nu z2 . ((mu z1 . (([@1]p1 | [@1]p2) | [@2][a][] z1)) & [@2][a][] z2)" \
  --point x
```

### Verbose + export game JSON for viewer

```bash
dune exec naive-modcheck-coalg -- \
  --logic relational \
  --model "[x : ({p1, p2}, [a : {x, y}]), y : ({p2}, [a : {x, y}])]" \
  --formula "nu z2 . ((mu z1 . (([@1]p1 | [@1]p2) | [@2][a][] z1)) & [@2][a][] z2)" \
  --point x \
  --verbose \
  --visualise \
  --generated/
```

Equivalent helper script:

```bash
sh test.sh
```

### Run the viewer

Serve the `viewer/` folder:

```bash
./run_viewer.sh
```

Then open:

- `http://localhost:8080`

Load an exported JSON from `generated/` using the **Load JSON** button.

## Test

### Run all tests

```bash
dune runtest
```

### Run model-checking suite only

```bash
dune runtest src/test/test_model_checking.exe
```

