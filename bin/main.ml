open! Core
open Naive_modcheck_coalg_parsers.Model

let () =
  let parsed = parse_relational_model "[s0: ({p}, [a: {s1, s2}])]" in
  print_s [%sexp (parsed : Relational_ast.t)]
