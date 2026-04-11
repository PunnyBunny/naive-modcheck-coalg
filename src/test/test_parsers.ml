open! Core
open OUnit2
module Model = Naive_modcheck_coalg_parsers.Model
module Formula = Naive_modcheck_coalg_parsers.Formula
module Logics = Naive_modcheck_coalg_logics.Logics

(* ============================================================================ *)
(* Test helpers *)
(* ============================================================================ *)

let assert_sexp ~expected actual =
  assert_equal ~cmp:Sexp.equal ~printer:Sexp.to_string_hum
    (Sexp.of_string expected)
    actual

let rel_formula s =
  Formula.parse_relational_formula s
  |> [%sexp_of: Formula.Relational_formula_parser.Ast.t]

let prob_formula s =
  Formula.parse_probabilistic_formula s
  |> [%sexp_of: Formula.Probabilistic_formula_parser.Ast.t]

module Rel_model =
  Naive_modcheck_coalg_parsers_model.Model_parser.Make
    (Model.Relational_parser.Model)

module Prob_model =
  Naive_modcheck_coalg_parsers_model.Model_parser.Make
    (Model.Probabilistic_parser.Model)

let rel_model s =
  Model.parse_relational_model s |> [%sexp_of: Rel_model.t]

let prob_model s =
  Model.parse_probabilistic_model s
  |> [%sexp_of: Prob_model.t]

let logics_prob_to_nnf s =
  Logics.Probabilistic.parse_formula s
  |> Logics.Probabilistic.formula_of_ast
  |> [%sexp_of: Logics.Probabilistic.Formula.t]

(* ============================================================================ *)
(* Relational Formulas *)
(* ============================================================================ *)

let test_rel_formula_literals _ =
  assert_sexp ~expected:"True" (rel_formula "true");
  assert_sexp ~expected:"False" (rel_formula "false");
  assert_sexp ~expected:"(Ap p)" (rel_formula "p");
  assert_sexp ~expected:"(Ap X)" (rel_formula "X")

let test_rel_formula_negation _ =
  assert_sexp ~expected:"(Not (Ap p))" (rel_formula "~p")

let test_rel_formula_conjunction _ =
  assert_sexp ~expected:"(And (Ap p) (Ap q))"
    (rel_formula "p && q");
  assert_sexp ~expected:"(And (Ap p) (Ap q))"
    (rel_formula "p ∧ q")

let test_rel_formula_disjunction _ =
  assert_sexp ~expected:"(Or (Ap p) (Ap q))"
    (rel_formula "p || q");
  assert_sexp ~expected:"(Or (Ap p) (Ap q))"
    (rel_formula "p ∨ q")

let test_rel_formula_modalities _ =
  assert_sexp ~expected:"(Modal (Right (Ap p)))"
    (rel_formula "<>p");
  assert_sexp ~expected:"(Modal (Left (Ap p)))"
    (rel_formula "[]p")

let test_rel_formula_fixpoints _ =
  assert_sexp
    ~expected:"(Mu X (Or (Ap p) (Modal (Right (Var X)))))"
    (rel_formula "mu X. p || <>X");
  assert_sexp
    ~expected:"(Mu X (Or (Ap p) (Modal (Right (Var X)))))"
    (rel_formula "μ X. p ∨ <>X");
  assert_sexp
    ~expected:"(Nu X (And (Ap p) (Modal (Left (Var X)))))"
    (rel_formula "nu X. p && []X");
  assert_sexp
    ~expected:"(Nu X (And (Ap p) (Modal (Left (Var X)))))"
    (rel_formula "ν X. p ∧ []X")

let test_rel_formula_associativity _ =
  assert_sexp
    ~expected:"(Or (And (Ap p) (Ap q)) (And (Ap r) (Ap s)))"
    (rel_formula "(p && q) || (r && s)");
  assert_sexp
    ~expected:
      "(Mu X (Or (And (Ap p) (Modal (Right (Var X)))) (Nu \
       Y (And (Ap q) (Modal (Left (Var Y)))))))"
    (rel_formula "mu X. (p && <>X) || nu Y. (q && []Y)");
  assert_sexp ~expected:"(And (And (Ap p) (Ap q)) (Ap r))"
    (rel_formula "p && q && r");
  assert_sexp ~expected:"(Or (Or (Ap p) (Ap q)) (Ap r))"
    (rel_formula "p || q || r");
  assert_sexp ~expected:"(And (Ap p) (And (Ap q) (Ap r)))"
    (rel_formula "p && (q && r)")

let test_rel_formula_mixed_unicode _ =
  assert_sexp
    ~expected:
      "(Mu X (And (Ap p) (Modal (Right (Or (Var X) (Nu Y \
       (Or (Ap q) (Modal (Left (Var Y))))))))))"
    (rel_formula "μ X. p ∧ <>X || ν Y. q ∨ []Y")

(* ============================================================================ *)
(* Relational Models *)
(* ============================================================================ *)

let test_rel_model_parsing _ =
  assert_sexp ~expected:"((s0 ((p) ((a (s1 s2))))))"
    (rel_model "[s0: ({p}, [a: {s1, s2}])]");
  assert_sexp ~expected:"()" (rel_model "[]");
  assert_sexp
    ~expected:"((s0 ((p q) ((a (s1))))) (s1 ((r) ())))"
    (rel_model "[s0: ({p, q}, [a: {s1}]), s1: ({r}, [])]");
  assert_sexp
    ~expected:"((s0 ((p) ((a (s1)) (b (s2 s3))))))"
    (rel_model "[s0: ({p}, [a: {s1}, b: {s2, s3}])]");
  assert_sexp ~expected:"((s0 ((p) ())))"
    (rel_model "[s0: ({p}, [])]")

(* ============================================================================ *)
(* Probabilistic Models *)
(* ============================================================================ *)

let test_prob_model_parsing _ =
  assert_sexp
    ~expected:"((s0 ((p) ((a ((s1 (1 2)) (s2 (1 2))))))))"
    (prob_model "[s0: ({p}, [a: [s1: 1/2, s2: 1/2]])]");
  assert_sexp ~expected:"()" (prob_model "[]");
  assert_sexp ~expected:"((s0 ((p) ((a ((s1 (1 1))))))))"
    (prob_model "[s0: ({p}, [a: [s1: 1/1]])]");
  assert_sexp
    ~expected:
      "((s0 ((p) ((a ((s1 (3 4)))) (b ((s2 (1 3))))))))"
    (prob_model "[s0: ({p}, [a: [s1: 3/4], b: [s2: 1/3]])]")

(* ============================================================================ *)
(* Probabilistic Formulas *)
(* ============================================================================ *)

let test_prob_formula_literals _ =
  assert_sexp ~expected:"True" (prob_formula "true");
  assert_sexp ~expected:"False" (prob_formula "false");
  assert_sexp ~expected:"(Ap p)" (prob_formula "p")

let test_prob_formula_negation _ =
  assert_sexp ~expected:"(Not (Ap p))" (prob_formula "~p")

let test_prob_formula_connectives _ =
  assert_sexp ~expected:"(And (Ap p) (Ap q))"
    (prob_formula "p && q");
  assert_sexp ~expected:"(Or (Ap p) (Ap q))"
    (prob_formula "p || q")

let test_prob_formula_modalities _ =
  assert_sexp ~expected:{|(Modal (Left ((1 2) (Ap p))))|}
    (prob_formula "<1/2> p");
  assert_sexp ~expected:{|(Modal (Right ((1 4) (Ap p))))|}
    (prob_formula "[1/4] p")

let test_prob_formula_fixpoints _ =
  assert_sexp
    ~expected:
      {|(Mu X (Or (Ap p) (Modal (Left ((1 2) (Var X))))))|}
    (prob_formula "mu X. p || <1/2>X");
  assert_sexp
    ~expected:
      {|(Nu X (And (Ap p) (Modal (Right ((1 2) (Var X))))))|}
    (prob_formula "nu X. p && [1/2]X")

let test_prob_formula_complex _ =
  assert_sexp
    ~expected:
      {|(Mu X (Or (And (Ap p) (Modal (Left ((1 2) (Var X))))) (Nu Y (And (Ap q) (Modal (Right ((1 3) (Var Y))))))))|}
    (prob_formula
       "mu X. (p && <1/2>X) || nu Y. (q && [1/3]Y)");
  assert_sexp
    ~expected:
      {|(Mu X (And (Ap p) (Modal (Left ((1 2) (Var X))))))|}
    (prob_formula "μ X. p ∧ <1/2>X")

(* ============================================================================ *)
(* Formula roundtrip (sexp) *)
(* ============================================================================ *)

let assert_sexp_roundtrip_rel input _ =
  let ast = Formula.parse_relational_formula input in
  let sexp =
    [%sexp_of: Formula.Relational_formula_parser.Ast.t] ast
  in
  let ast' =
    [%of_sexp: Formula.Relational_formula_parser.Ast.t] sexp
  in
  let sexp' =
    [%sexp_of: Formula.Relational_formula_parser.Ast.t] ast'
  in
  assert_equal ~cmp:Sexp.equal ~printer:Sexp.to_string_hum
    sexp sexp'

let assert_sexp_roundtrip_prob input _ =
  let ast = Formula.parse_probabilistic_formula input in
  let sexp =
    [%sexp_of: Formula.Probabilistic_formula_parser.Ast.t]
      ast
  in
  let ast' =
    [%of_sexp: Formula.Probabilistic_formula_parser.Ast.t]
      sexp
  in
  let sexp' =
    [%sexp_of: Formula.Probabilistic_formula_parser.Ast.t]
      ast'
  in
  assert_equal ~cmp:Sexp.equal ~printer:Sexp.to_string_hum
    sexp sexp'

(* ============================================================================ *)
(* Model roundtrip *)
(* ============================================================================ *)

let roundtrip_rel input =
  let model = Model.parse_relational_model input in
  let pp = Rel_model.pretty_print model in
  let model' = Model.parse_relational_model pp in
  ( [%sexp_of: Rel_model.t] model
  , [%sexp_of: Rel_model.t] model' )

let roundtrip_prob input =
  let model = Model.parse_probabilistic_model input in
  let pp = Prob_model.pretty_print model in
  let model' = Model.parse_probabilistic_model pp in
  ( [%sexp_of: Prob_model.t] model
  , [%sexp_of: Prob_model.t] model' )

let assert_roundtrip_rel input _ =
  let before, after = roundtrip_rel input in
  assert_equal ~cmp:Sexp.equal ~printer:Sexp.to_string_hum
    before after

let assert_roundtrip_prob input _ =
  let before, after = roundtrip_prob input in
  assert_equal ~cmp:Sexp.equal ~printer:Sexp.to_string_hum
    before after

let test_rel_model_roundtrip_empty _ =
  assert_sexp ~expected:"()" (rel_model "[]")

let test_rel_model_roundtrip_single _ =
  assert_sexp ~expected:"((s0 ((p) ((a (s1 s2))))))"
    (rel_model "[s0: ({p}, [a: {s1, s2}])]")

let test_rel_model_roundtrip_multi_states _ =
  assert_sexp
    ~expected:"((s0 ((p q) ((a (s1))))) (s1 ((r) ())))"
    (rel_model "[s0: ({p, q}, [a: {s1}]), s1: ({r}, [])]")

let test_rel_model_roundtrip_multi_actions _ =
  assert_sexp
    ~expected:"((s0 ((p) ((a (s1)) (b (s2 s3))))))"
    (rel_model "[s0: ({p}, [a: {s1}, b: {s2, s3}])]")

let test_rel_model_roundtrip_empty_aps _ =
  assert_sexp ~expected:"((s0 (() ((a (s1))))))"
    (rel_model "[s0: ({}, [a: {s1}])]")

let test_rel_model_roundtrip_empty_action _ =
  assert_sexp ~expected:{|((x ((p1) (("" (x y))))))|}
    (rel_model "[x: ({p1}, [{}: {x, y}])]")

let test_prob_model_roundtrip_empty _ =
  assert_sexp ~expected:"()" (prob_model "[]")

let test_prob_model_roundtrip_single _ =
  assert_sexp
    ~expected:"((s0 ((p) ((a ((s1 (1 2)) (s2 (1 2))))))))"
    (prob_model "[s0: ({p}, [a: [s1: 1/2, s2: 1/2]])]")

let test_prob_model_roundtrip_multi_actions _ =
  assert_sexp
    ~expected:
      "((s0 ((p) ((a ((s1 (3 4)))) (b ((s2 (1 3))))))))"
    (prob_model "[s0: ({p}, [a: [s1: 3/4], b: [s2: 1/3]])]")

(* ============================================================================ *)
(* NNF conversion *)
(* ============================================================================ *)

let test_prob_nnf _ =
  assert_sexp ~expected:"(Or (Not p) (Not q))"
    (logics_prob_to_nnf "~(p && q)");
  assert_sexp ~expected:{|(Modal (Right ((1 2) (Not p))))|}
    (logics_prob_to_nnf "~<1/2>p");
  assert_sexp
    ~expected:
      {|(Mu X (Or (And (Ap p) (Modal (Left ((1 2) (Var X))))) (Nu Y (And (Ap q) (Modal (Right ((1 3) (Var Y))))))))|}
    (logics_prob_to_nnf
       "mu X. (p && <1/2>X) || nu Y. (q && [1/3]Y)")

(* ============================================================================ *)
(* Suite *)
(* ============================================================================ *)

let suite =
  "Parsing"
  >::: [
         "Relational formulas"
         >::: [
                "literals" >:: test_rel_formula_literals
              ; "negation" >:: test_rel_formula_negation
              ; "conjunction"
                >:: test_rel_formula_conjunction
              ; "disjunction"
                >:: test_rel_formula_disjunction
              ; "modalities" >:: test_rel_formula_modalities
              ; "fixpoints" >:: test_rel_formula_fixpoints
              ; "associativity"
                >:: test_rel_formula_associativity
              ; "mixed Unicode"
                >:: test_rel_formula_mixed_unicode
              ; "sexp roundtrip: literal"
                >:: assert_sexp_roundtrip_rel "true"
              ; "sexp roundtrip: modality"
                >:: assert_sexp_roundtrip_rel "<>p"
              ; "sexp roundtrip: fixpoint"
                >:: assert_sexp_roundtrip_rel
                      "mu X. p || <>X"
              ; "sexp roundtrip: complex"
                >:: assert_sexp_roundtrip_rel
                      "mu X. (p && <>X) || nu Y. (q && []Y)"
              ]
       ; "Relational models"
         >::: [
                "parsing" >:: test_rel_model_parsing
              ; "roundtrip: empty"
                >:: test_rel_model_roundtrip_empty
              ; "roundtrip: single state"
                >:: test_rel_model_roundtrip_single
              ; "roundtrip: multiple states"
                >:: test_rel_model_roundtrip_multi_states
              ; "roundtrip: multiple actions"
                >:: test_rel_model_roundtrip_multi_actions
              ; "roundtrip: empty aps"
                >:: test_rel_model_roundtrip_empty_aps
              ; "roundtrip: empty action"
                >:: test_rel_model_roundtrip_empty_action
              ; "pp roundtrip: single state"
                >:: assert_roundtrip_rel
                      "[s0: ({p}, [a: {s1, s2}])]"
              ; "pp roundtrip: multiple states"
                >:: assert_roundtrip_rel
                      "[s0: ({p, q}, [a: {s1}]), s1: ({r}, \
                       [])]"
              ; "pp roundtrip: multiple actions"
                >:: assert_roundtrip_rel
                      "[s0: ({p}, [a: {s1}, b: {s2, s3}])]"
              ; "pp roundtrip: empty aps"
                >:: assert_roundtrip_rel
                      "[s0: ({}, [a: {s1}])]"
              ; "pp roundtrip: empty action"
                >:: assert_roundtrip_rel
                      "[x: ({p1}, [{}: {x, y}])]"
              ]
       ; "Probabilistic formulas"
         >::: [
                "literals" >:: test_prob_formula_literals
              ; "negation" >:: test_prob_formula_negation
              ; "connectives"
                >:: test_prob_formula_connectives
              ; "modalities"
                >:: test_prob_formula_modalities
              ; "fixpoints" >:: test_prob_formula_fixpoints
              ; "complex" >:: test_prob_formula_complex
              ; "sexp roundtrip: modality"
                >:: assert_sexp_roundtrip_prob "<1/2> p"
              ; "sexp roundtrip: fixpoint"
                >:: assert_sexp_roundtrip_prob
                      "mu X. p || <1/2>X"
              ; "sexp roundtrip: complex"
                >:: assert_sexp_roundtrip_prob
                      "mu X. (p && <1/2>X) || nu Y. (q && \
                       [1/3]Y)"
              ]
       ; "Probabilistic models"
         >::: [
                "parsing" >:: test_prob_model_parsing
              ; "roundtrip: empty"
                >:: test_prob_model_roundtrip_empty
              ; "roundtrip: single state"
                >:: test_prob_model_roundtrip_single
              ; "roundtrip: multiple actions"
                >:: test_prob_model_roundtrip_multi_actions
              ; "pp roundtrip: single state"
                >:: assert_roundtrip_prob
                      "[s0: ({p}, [a: [s1: 1/2, s2: 1/2]])]"
              ; "pp roundtrip: multiple actions"
                >:: assert_roundtrip_prob
                      "[s0: ({p}, [a: [s1: 3/4], b: [s2: \
                       1/3]])]"
              ]
       ; "NNF conversion"
         >::: [ "probabilistic" >:: test_prob_nnf ]
       ]

let () = run_test_tt_main suite
