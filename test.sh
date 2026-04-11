dune exec naive-modcheck-coalg -- \
  --model "[x : ({p1, p2}, [a : {x, y}]), y : ({p2}, [a : {x, y}])]" \
  --formula " nu z2 . ((mu z1 . (([@1]p1 | [@1]p2) | [@2][a][] z1)) & [@2][a][] z2) " \
  --point x \
  --logic relational \
  --verbose --visualise generated

