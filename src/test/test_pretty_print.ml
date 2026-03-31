open! Core
open OUnit2
module Model = Naive_modcheck_coalg_parsers.Model
module Model_ast = Naive_modcheck_coalg_parsers.Model.Ast

let roundtrip_rel model_str _ =
  let model = Model.parse_relational_model model_str in
  let pp = Model_ast.Relational_ast.pretty_print model in
  let model' = Model.parse_relational_model pp in
  assert_equal ~printer:Sexp.to_string_hum
    ([%sexp_of: Model_ast.Relational_ast.t] model)
    ([%sexp_of: Model_ast.Relational_ast.t] model')

let roundtrip_prob model_str _ =
  let model = Model.parse_probabilistic_model model_str in
  let pp = Model_ast.Probabilistic_ast.pretty_print model in
  let model' = Model.parse_probabilistic_model pp in
  assert_equal ~printer:Sexp.to_string_hum
    ([%sexp_of: Model_ast.Probabilistic_ast.t] model)
    ([%sexp_of: Model_ast.Probabilistic_ast.t] model')

let suite =
  "Pretty_print"
  >::: [
         "relational: empty" >:: roundtrip_rel "[]"
       ; "relational: single state"
         >:: roundtrip_rel "[s0: ({p}, [a: {s1, s2}])]"
       ; "relational: multiple states"
         >:: roundtrip_rel
               "[s0: ({p, q}, [a: {s1}]), s1: ({r}, [])]"
       ; "relational: multiple actions"
         >:: roundtrip_rel
               "[s0: ({p}, [a: {s1}, b: {s2, s3}])]"
       ; "relational: empty aps"
         >:: roundtrip_rel "[s0: ({}, [a: {s1}])]"
       ; "relational: empty action"
         >:: roundtrip_rel "[x: ({p1}, [{}: {x, y}])]"
       ; "probabilistic: empty" >:: roundtrip_prob "[]"
       ; "probabilistic: single state"
         >:: roundtrip_prob
               "[s0: ({p}, [a: [s1: 1/2, s2: 1/2]])]"
       ; "probabilistic: multiple actions"
         >:: roundtrip_prob
               "[s0: ({p}, [a: [s1: 3/4], b: [s2: 1/3]])]"
       ]

let () = run_test_tt_main suite
