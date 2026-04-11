open! Core
open OUnit2
module Logics = Naive_modcheck_coalg_logics.Logics

let roundtrip_rel model_str _ =
  let model = Logics.Relational.parse_model model_str in
  let model' = Logics.Relational.parse_model model_str in
  assert_equal ~printer:Sexp.to_string_hum
    ([%sexp_of: Logics.Relational.Model.t] model)
    ([%sexp_of: Logics.Relational.Model.t] model')

let roundtrip_prob model_str _ =
  let model = Logics.Probabilistic.parse_model model_str in
  let model' = Logics.Probabilistic.parse_model model_str in
  assert_equal ~printer:Sexp.to_string_hum
    ([%sexp_of: Logics.Probabilistic.Model.t] model)
    ([%sexp_of: Logics.Probabilistic.Model.t] model')

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
