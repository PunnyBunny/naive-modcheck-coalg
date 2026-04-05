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
  |> [%sexp_of: Formula.Ast.Relational_ast.t]

let prob_formula s =
  Formula.parse_probabilistic_formula s
  |> [%sexp_of: Formula.Ast.Probabilistic_ast.t]

let rel_model s =
  Model.parse_relational_model s
  |> [%sexp_of: Model.Ast.Relational_ast.t]

let prob_model s =
  Model.parse_probabilistic_model s
  |> [%sexp_of: Model.Ast.Probabilistic_ast.t]

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
  assert_sexp ~expected:"(Modal (Diamond (Ap p)))"
    (rel_formula "<>p");
  assert_sexp ~expected:"(Modal (Box (Ap p)))"
    (rel_formula "[]p")

let test_rel_formula_fixpoints _ =
  assert_sexp
    ~expected:"(Mu X (Or (Ap p) (Modal (Diamond (Var X)))))"
    (rel_formula "mu X. p || <>X");
  assert_sexp
    ~expected:"(Mu X (Or (Ap p) (Modal (Diamond (Var X)))))"
    (rel_formula "μ X. p ∨ <>X");
  assert_sexp
    ~expected:"(Nu X (And (Ap p) (Modal (Box (Var X)))))"
    (rel_formula "nu X. p && []X");
  assert_sexp
    ~expected:"(Nu X (And (Ap p) (Modal (Box (Var X)))))"
    (rel_formula "ν X. p ∧ []X")

let test_rel_formula_associativity _ =
  assert_sexp
    ~expected:"(Or (And (Ap p) (Ap q)) (And (Ap r) (Ap s)))"
    (rel_formula "(p && q) || (r && s)");
  assert_sexp
    ~expected:
      "(Mu X (Or (And (Ap p) (Modal (Diamond (Var X)))) (Nu Y \
       (And (Ap q) (Modal (Box (Var Y)))))))"
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
      "(Mu X (And (Ap p) (Modal (Diamond (Or (Var X) (Nu Y \
       (Or (Ap q) (Modal (Box (Var Y))))))))))"
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
  assert_sexp ~expected:{|(Modal (GT (1 2) (Ap p)))|}
    (prob_formula "<1/2> p");
  assert_sexp ~expected:{|(Modal (LE (1 4) (Ap p)))|}
    (prob_formula "[1/4] p")

let test_prob_formula_fixpoints _ =
  assert_sexp
    ~expected:
      {|(Mu X (Or (Ap p) (Modal (GT (1 2) (Var X)))))|}
    (prob_formula "mu X. p || <1/2>X");
  assert_sexp
    ~expected:
      {|(Nu X (And (Ap p) (Modal (LE (1 2) (Var X)))))|}
    (prob_formula "nu X. p && [1/2]X")

let test_prob_formula_complex _ =
  assert_sexp
    ~expected:
      {|(Mu X (Or (And (Ap p) (Modal (GT (1 2) (Var X)))) (Nu Y (And (Ap q) (Modal (LE (1 3) (Var Y)))))))|}
    (prob_formula
       "mu X. (p && <1/2>X) || nu Y. (q && [1/3]Y)");
  assert_sexp
    ~expected:
      {|(Mu X (And (Ap p) (Modal (GT (1 2) (Var X)))))|}
    (prob_formula "μ X. p ∧ <1/2>X")

(* ============================================================================ *)
(* NNF conversion *)
(* ============================================================================ *)

let test_prob_nnf _ =
  assert_sexp ~expected:"(Or (Not p) (Not q))"
    (logics_prob_to_nnf "~(p && q)");
  assert_sexp ~expected:{|(Modal (LE (1 2) (Not p)))|}
    (logics_prob_to_nnf "~<1/2>p");
  assert_sexp
    ~expected:
      {|(Mu X (Or (And (Ap p) (Modal (GT (1 2) (Var X)))) (Nu Y (And (Ap q) (Modal (LE (1 3) (Var Y)))))))|}
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
              ]
       ; "Relational models"
         >::: [ "parsing" >:: test_rel_model_parsing ]
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
              ]
       ; "Probabilistic models"
         >::: [ "parsing" >:: test_prob_model_parsing ]
       ; "NNF conversion"
         >::: [ "probabilistic" >:: test_prob_nnf ]
       ]

let () = run_test_tt_main suite
