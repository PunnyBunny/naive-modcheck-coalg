open! Core
module Model = Naive_modcheck_coalg_parsers.Model
module Formula = Naive_modcheck_coalg_parsers.Formula
module Logics = Naive_modcheck_coalg_logics.Logics

(* ============================================================================ *)
(* Test helpers *)
(* ============================================================================ *)

let rel_formula s =
  Formula.parse_relational_formula s
  |> [%sexp_of: Formula.Ast.Relational_ast.t] |> print_s

let prob_formula s =
  Formula.parse_probabilistic_formula s
  |> [%sexp_of: Formula.Ast.Probabilistic_ast.t] |> print_s

let rel_model s =
  Model.parse_relational_model s
  |> [%sexp_of: Model.Ast.Relational_ast.t] |> print_s

let prob_model s =
  Model.parse_probabilistic_model s
  |> [%sexp_of: Model.Ast.Probabilistic_ast.t] |> print_s

let logics_rel_formula s =
  Logics.Relational.parse_formula s
  |> [%sexp_of: Logics.Relational.Formula_ast.t] |> print_s

let logics_rel_model s =
  Logics.Relational.parse_model s
  |> [%sexp_of: Logics.Relational.Model.t] |> print_s

let logics_prob_formula s =
  Logics.Probabilistic.parse_formula s
  |> [%sexp_of: Logics.Probabilistic.Formula_ast.t] |> print_s

let logics_prob_model s =
  Logics.Probabilistic.parse_model s
  |> [%sexp_of: Logics.Probabilistic.Model.t] |> print_s

let logics_prob_to_nnf s =
  Logics.Probabilistic.parse_formula s
  |> Logics.Probabilistic.formula_of_ast
  |> [%sexp_of: Logics.Probabilistic.Formula.t] |> print_s

(* ============================================================================ *)
(* Relational Formulas *)
(* ============================================================================ *)

let%expect_test "relational formula: literals" =
  rel_formula "true";
  [%expect {| True |}];
  rel_formula "false";
  [%expect {| False |}];
  rel_formula "p";
  [%expect {| (Ap p) |}];
  rel_formula "X";
  [%expect {| (Ap X) |}]

let%expect_test "relational formula: negation" =
  rel_formula "~p";
  [%expect {| (Not (Ap p)) |}]

let%expect_test "relational formula: conjunction" =
  rel_formula "p && q";
  [%expect {| (And (Ap p) (Ap q)) |}];
  rel_formula "p ∧ q";
  [%expect {| (And (Ap p) (Ap q)) |}]

let%expect_test "relational formula: disjunction" =
  rel_formula "p || q";
  [%expect {| (Or (Ap p) (Ap q)) |}];
  rel_formula "p ∨ q";
  [%expect {| (Or (Ap p) (Ap q)) |}]

let%expect_test "relational formula: modalities" =
  rel_formula "<a>p";
  [%expect {| (Diamond a () (Ap p)) |}];
  rel_formula "[a]p";
  [%expect {| (Box a () (Ap p)) |}]

let%expect_test "relational formula: fixpoints" =
  rel_formula "mu X. p || <a>X";
  [%expect {| (Mu X (Or (Ap p) (Diamond a () (Ap X)))) |}];
  rel_formula "μ X. p ∨ <a>X";
  [%expect {| (Mu X (Or (Ap p) (Diamond a () (Ap X)))) |}];
  rel_formula "nu X. p && [a]X";
  [%expect {| (Nu X (And (Ap p) (Box a () (Ap X)))) |}];
  rel_formula "ν X. p ∧ [a]X";
  [%expect {| (Nu X (And (Ap p) (Box a () (Ap X)))) |}]

let%expect_test "relational formula: associativity and nesting" =
  rel_formula "(p && q) || (r && s)";
  [%expect {| (Or (And (Ap p) (Ap q)) (And (Ap r) (Ap s))) |}];
  rel_formula "mu X. (p && <a>X) || nu Y. (q && [b]Y)";
  [%expect
    {|
    (Mu X
     (Or (And (Ap p) (Diamond a () (Ap X)))
      (Nu Y (And (Ap q) (Box b () (Ap Y))))))
    |}];
  rel_formula "p && q && r";
  [%expect {| (And (And (Ap p) (Ap q)) (Ap r)) |}];
  rel_formula "p || q || r";
  [%expect {| (Or (Or (Ap p) (Ap q)) (Ap r)) |}];
  rel_formula "p && (q && r)";
  [%expect {| (And (Ap p) (And (Ap q) (Ap r))) |}]

let%expect_test "relational formula: mixed Unicode and ASCII" =
  rel_formula "μ X. p ∧ <a>X || ν Y. q ∨ [b]Y";
  [%expect
    {|
    (Mu X
     (And (Ap p) (Diamond a () (Or (Ap X) (Nu Y (Or (Ap q) (Box b () (Ap Y))))))))
    |}]

(* ============================================================================ *)
(* Relational Models *)
(* ============================================================================ *)

let%expect_test "relational model: parsing" =
  rel_model "[s0: ({p}, [a: {s1, s2}])]";
  [%expect {| ((s0 ((p) ((a (s1 s2)))))) |}];
  rel_model "[]";
  [%expect {| () |}];
  rel_model "[s0: ({p, q}, [a: {s1}]), s1: ({r}, [])]";
  [%expect {| ((s0 ((p q) ((a (s1))))) (s1 ((r) ()))) |}];
  rel_model "[s0: ({p}, [a: {s1}, b: {s2, s3}])]";
  [%expect {| ((s0 ((p) ((a (s1)) (b (s2 s3)))))) |}];
  rel_model "[s0: ({p}, [])]";
  [%expect {| ((s0 ((p) ()))) |}]

(* ============================================================================ *)
(* Probabilistic Models *)
(* ============================================================================ *)

let%expect_test "probabilistic model: parsing" =
  prob_model "[s0: ({p}, [a: [s1: 1/2, s2: 1/2]])]";
  [%expect {| ((s0 ((p) ((a ((s1 (1 2)) (s2 (1 2)))))))) |}];
  prob_model "[]";
  [%expect {| () |}];
  prob_model "[s0: ({p}, [a: [s1: 1/1]])]";
  [%expect {| ((s0 ((p) ((a ((s1 (1 1)))))))) |}];
  prob_model "[s0: ({p}, [a: [s1: 3/4], b: [s2: 1/3]])]";
  [%expect {| ((s0 ((p) ((a ((s1 (3 4)))) (b ((s2 (1 3)))))))) |}]

(* ============================================================================ *)
(* Probabilistic Formulas *)
(* ============================================================================ *)

let%expect_test "probabilistic formula: literals" =
  prob_formula "true";
  [%expect {| True |}];
  prob_formula "false";
  [%expect {| False |}];
  prob_formula "p";
  [%expect {| (Ap p) |}]

let%expect_test "probabilistic formula: negation" =
  prob_formula "~p";
  [%expect {| (Not (Ap p)) |}]

let%expect_test "probabilistic formula: connectives" =
  prob_formula "p && q";
  [%expect {| (And (Ap p) (Ap q)) |}];
  prob_formula "p || q";
  [%expect {| (Or (Ap p) (Ap q)) |}]

let%expect_test "probabilistic formula: modalities with thresholds" =
  prob_formula "<1/2> p";
  [%expect {| (Diamond "" (1 2) (Ap p)) |}];
  prob_formula "[1/4] p";
  [%expect {| (Box "" (1 4) (Ap p)) |}];
  prob_formula "<1/2 a> p";
  [%expect {| (Diamond a (1 2) (Ap p)) |}];
  prob_formula "[3/4 a] p";
  [%expect {| (Box a (3 4) (Ap p)) |}]

let%expect_test "probabilistic formula: fixpoints" =
  prob_formula "mu X. p || <1/2>X";
  [%expect {| (Mu X (Or (Ap p) (Diamond "" (1 2) (Ap X)))) |}];
  prob_formula "nu X. p && [1/2]X";
  [%expect {| (Nu X (And (Ap p) (Box "" (1 2) (Ap X)))) |}]

let%expect_test "probabilistic formula: complex and Unicode" =
  prob_formula "mu X. (p && <1/2>X) || nu Y. (q && [1/3]Y)";
  [%expect
    {|
    (Mu X
     (Or (And (Ap p) (Diamond "" (1 2) (Ap X)))
      (Nu Y (And (Ap q) (Box "" (1 3) (Ap Y))))))
    |}];
  prob_formula "μ X. p ∧ <1/2>X";
  [%expect {| (Mu X (And (Ap p) (Diamond "" (1 2) (Ap X)))) |}]

(* ============================================================================ *)
(* Logics Module Tests *)
(* ============================================================================ *)

let%expect_test "Logics.Relational: parse formula and model" =
  logics_rel_formula "<a>p";
  [%expect {| (Diamond a () (Ap p)) |}];
  logics_rel_model "[s0: ({p}, [a: {s1}])]";
  [%expect {| ((s0 ((p) ((a (s1)))))) |}]

let%expect_test "Logics.Probabilistic: parse formula" =
  logics_prob_formula "<1/2>p";
  [%expect {| (Diamond "" (1 2) (Ap p)) |}];
  logics_prob_formula "[1/3]p";
  [%expect {| (Box "" (1 3) (Ap p)) |}];
  logics_prob_formula "mu X. p || <1/2>X";
  [%expect {| (Mu X (Or (Ap p) (Diamond "" (1 2) (Ap X)))) |}];
  logics_prob_formula "nu X. p && [3/4]X";
  [%expect {| (Nu X (And (Ap p) (Box "" (3 4) (Ap X)))) |}]

let%expect_test "Logics.Probabilistic: parse model" =
  logics_prob_model "[s0: ({p}, [a: [s1: 1/2, s2: 1/2]])]";
  [%expect {| ((s0 ((p) ((a ((s1 (1 2)) (s2 (1 2)))))))) |}]

let%expect_test "Logics.Probabilistic: formula_of_ast (NNF conversion)" =
  logics_prob_to_nnf "~(p && q)";
  [%expect {| (Or (Not p) (Not q)) |}];
  logics_prob_to_nnf "~<1/2>p";
  [%expect {| (Box "" (1 2) (Not p)) |}];
  logics_prob_to_nnf "mu X. (p && <1/2>X) || nu Y. (q && [1/3]Y)";
  [%expect
    {|
    (Mu X
     (Or (And (Ap p) (Diamond "" (1 2) (Ap X)))
      (Nu Y (And (Ap q) (Box "" (1 3) (Ap Y))))))
    |}]
