## Run

Viewer: `python3 -m http.server -d viewer 8080`

Model checking: for example,
```
dune exec naive-modcheck-coalg -- \
  --logic relational \
  --model '[x:({p1},[{} : {x, y}]), y:({p2},[{} : {x, y}])]' \
  --formula 'ν x2.(μ x1 .((p1 & <> x1) | (p2 & [] x2)))' \
  --point x \
  --visualise ./generated/
```