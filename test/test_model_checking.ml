open! Core
open OUnit2
open Naive_modcheck_coalg_common
open Naive_modcheck_coalg_logics
open Naive_modcheck_coalg_checker

let check_rel ~model_str ~formula_str ~point ~expected =
  let model = Logics.Relational.parse_model model_str in
  let formula_ast =
    Logics.Relational.parse_formula formula_str
  in
  let formula =
    Logics.Relational.formula_of_ast formula_ast
  in
  let result =
    Checkers.Relational.model_check ~verbose:false ~model
      ~point:(State.of_string point)
      ~formula
  in
  assert_equal ~printer:Bool.to_string expected result

let check_prob ~model_str ~formula_str ~point ~expected =
  let model = Logics.Probabilistic.parse_model model_str in
  let formula_ast =
    Logics.Probabilistic.parse_formula formula_str
  in
  let formula =
    Logics.Probabilistic.formula_of_ast formula_ast
  in
  let result =
    Checkers.Probabilistic.model_check ~verbose:false ~model
      ~point:(State.of_string point)
      ~formula
  in
  assert_equal ~printer:Bool.to_string expected result

(* ============================================================================ *)
(* Probabilistic Model Checking *)
(* ============================================================================ *)

let prob_model_1 =
  {|[x: ({p}, [a: [x: 2/3], b: [y: 1/3]]),
     y: ({ },  [a: [x: 1/3], b: [y: 2/3]])]|}

let prob_model_2 =
  {|[x: ({p}, [a: [x: 2/3, y: 1/3]]),
     y: ({ },  [a: [x: 1/2, y: 1/2]])]|}

let test_prob_case_1 _ =
  check_prob ~model_str:prob_model_1
    ~formula_str:"nu x . (p & <1/2 a> x)" ~point:"x"
    ~expected:true;
  check_prob ~model_str:prob_model_1
    ~formula_str:"nu x . (~p & [1/2 b] ~x)" ~point:"x"
    ~expected:false

let test_prob_case_2 _ =
  check_prob ~model_str:prob_model_2
    ~formula_str:"nu x . (p & <1/2 a> x)" ~point:"x"
    ~expected:true;
  check_prob ~model_str:prob_model_2
    ~formula_str:"nu x . (~p & [1/2 b] ~x)" ~point:"x"
    ~expected:false

(* ============================================================================ *)
(* Relational Model Checking *)
(* ============================================================================ *)

let rel_model_1 =
  {|[x: ({p1}, [{}: {x, y}]),
     y: ({p2}, [{}: {x, y}])]|}

let rel_model_2 =
  {|[x: ({ }, [{}: {x}, bubi: {y}]),
     y: ({ }, [])]|}

let test_rel_case_1 _ =
  check_rel ~model_str:rel_model_1
    ~formula_str:
      "nu x2 . (mu x1 . ((p1 & <> x1) | (p2 & [] x2)))"
    ~point:"x" ~expected:true

let test_rel_case_2 _ =
  check_rel ~model_str:rel_model_2
    ~formula_str:
      "nu x2 . (mu x1 . ((p1 & <> x1) | (p2 & [] x2)))"
    ~point:"x" ~expected:false

(* ============================================================================ *)
(* Suite *)
(* ============================================================================ *)

let suite =
  "Model_checking"
  >::: [
         "probabilistic: case 1" >:: test_prob_case_1
       ; "probabilistic: case 2" >:: test_prob_case_2
       ; "relational: case 1" >:: test_rel_case_1
       ; "relational: case 2" >:: test_rel_case_2
       ]

let () = run_test_tt_main suite
