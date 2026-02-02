open! Core
module Model = Naive_modcheck_coalg_parsers.Model
module Formula = Naive_modcheck_coalg_parsers.Formula

(* ============================================================================ *)
(* Relational Formulas *)
(* ============================================================================ *)

let%expect_test "formula: true" =
  let parsed = Formula.parse_relational_formula "true" in
  print_s [%sexp (parsed : Formula.Ast.Relational_ast.t)];
  [%expect {| True |}]

let%expect_test "formula: false" =
  let parsed = Formula.parse_relational_formula "false" in
  print_s [%sexp (parsed : Formula.Ast.Relational_ast.t)];
  [%expect {| False |}]

let%expect_test "formula: atomic proposition" =
  let parsed = Formula.parse_relational_formula "p" in
  print_s [%sexp (parsed : Formula.Ast.Relational_ast.t)];
  [%expect {| (Ap p) |}]

let%expect_test "formula: negation" =
  let parsed = Formula.parse_relational_formula "~p" in
  print_s [%sexp (parsed : Formula.Ast.Relational_ast.t)];
  [%expect {| (Not (Ap p)) |}]

let%expect_test "formula: conjunction (ASCII)" =
  let parsed = Formula.parse_relational_formula "p && q" in
  print_s [%sexp (parsed : Formula.Ast.Relational_ast.t)];
  [%expect {| (And (Ap p) (Ap q)) |}]

let%expect_test "formula: conjunction (Unicode)" =
  let parsed = Formula.parse_relational_formula "p ∧ q" in
  print_s [%sexp (parsed : Formula.Ast.Relational_ast.t)];
  [%expect {| (And (Ap p) (Ap q)) |}]

let%expect_test "formula: disjunction (ASCII)" =
  let parsed = Formula.parse_relational_formula "p || q" in
  print_s [%sexp (parsed : Formula.Ast.Relational_ast.t)];
  [%expect {| (Or (Ap p) (Ap q)) |}]

let%expect_test "formula: disjunction (Unicode)" =
  let parsed = Formula.parse_relational_formula "p ∨ q" in
  print_s [%sexp (parsed : Formula.Ast.Relational_ast.t)];
  [%expect {| (Or (Ap p) (Ap q)) |}]

let%expect_test "formula: diamond" =
  let parsed = Formula.parse_relational_formula "<a>p" in
  print_s [%sexp (parsed : Formula.Ast.Relational_ast.t)];
  [%expect {| (Diamond a () (Ap p)) |}]

let%expect_test "formula: box" =
  let parsed = Formula.parse_relational_formula "[a]p" in
  print_s [%sexp (parsed : Formula.Ast.Relational_ast.t)];
  [%expect {| (Box a () (Ap p)) |}]

let%expect_test "formula: mu (ASCII)" =
  let parsed = Formula.parse_relational_formula "mu X. p || <a>X" in
  print_s [%sexp (parsed : Formula.Ast.Relational_ast.t)];
  [%expect {| (Mu X (Or (Ap p) (Diamond a () (Ap X)))) |}]

let%expect_test "formula: mu (Unicode)" =
  let parsed = Formula.parse_relational_formula "μ X. p ∨ <a>X" in
  print_s [%sexp (parsed : Formula.Ast.Relational_ast.t)];
  [%expect {| (Mu X (Or (Ap p) (Diamond a () (Ap X)))) |}]

let%expect_test "formula: nu (ASCII)" =
  let parsed = Formula.parse_relational_formula "nu X. p && [a]X" in
  print_s [%sexp (parsed : Formula.Ast.Relational_ast.t)];
  [%expect {| (Nu X (And (Ap p) (Box a () (Ap X)))) |}]

let%expect_test "formula: nu (Unicode)" =
  let parsed = Formula.parse_relational_formula "ν X. p ∧ [a]X" in
  print_s [%sexp (parsed : Formula.Ast.Relational_ast.t)];
  [%expect {| (Nu X (And (Ap p) (Box a () (Ap X)))) |}]

let%expect_test "formula: variable" =
  let parsed = Formula.parse_relational_formula "X" in
  print_s [%sexp (parsed : Formula.Ast.Relational_ast.t)];
  [%expect {| (Ap X) |}]

let%expect_test "formula: nested operators" =
  let parsed = Formula.parse_relational_formula "(p && q) || (r && s)" in
  print_s [%sexp (parsed : Formula.Ast.Relational_ast.t)];
  [%expect {| (Or (And (Ap p) (Ap q)) (And (Ap r) (Ap s))) |}]

let%expect_test "formula: complex nested mu-calculus" =
  let parsed =
    Formula.parse_relational_formula "mu X. (p && <a>X) || nu Y. (q && [b]Y)"
  in
  print_s [%sexp (parsed : Formula.Ast.Relational_ast.t)];
  [%expect
    {|
    (Mu X
     (Or (And (Ap p) (Diamond a () (Ap X)))
      (Nu Y (And (Ap q) (Box b () (Ap Y))))))
    |}]

let%expect_test "formula: left associativity of conjunction" =
  let parsed = Formula.parse_relational_formula "p && q && r" in
  print_s [%sexp (parsed : Formula.Ast.Relational_ast.t)];
  [%expect {| (And (And (Ap p) (Ap q)) (Ap r)) |}]

let%expect_test "formula: left associativity of disjunction" =
  let parsed = Formula.parse_relational_formula "p || q || r" in
  print_s [%sexp (parsed : Formula.Ast.Relational_ast.t)];
  [%expect {| (Or (Or (Ap p) (Ap q)) (Ap r)) |}]

let%expect_test "formula: parentheses override associativity" =
  let parsed = Formula.parse_relational_formula "p && (q && r)" in
  print_s [%sexp (parsed : Formula.Ast.Relational_ast.t)];
  [%expect {| (And (Ap p) (And (Ap q) (Ap r))) |}]

let%expect_test "formula: mixed Unicode and ASCII" =
  let parsed =
    Formula.parse_relational_formula "μ X. p ∧ <a>X || ν Y. q ∨ [b]Y"
  in
  print_s [%sexp (parsed : Formula.Ast.Relational_ast.t)];
  [%expect
    {|
    (Mu X
     (And (Ap p) (Diamond a () (Or (Ap X) (Nu Y (Or (Ap q) (Box b () (Ap Y))))))))
    |}]

let%expect_test "formula: negation with atom" =
  let parsed = Formula.parse_relational_formula "~p" in
  print_s [%sexp (parsed : Formula.Ast.Relational_ast.t)];
  [%expect {| (Not (Ap p)) |}]

(* ============================================================================ *)
(* Relational Models *)
(* ============================================================================ *)

let%expect_test "relational: basic model" =
  let parsed = Model.parse_relational_model "[s0: ({p}, [a: {s1, s2}])]" in
  print_s [%sexp (parsed : Model.Ast.Relational_ast.t)];
  [%expect {| ((s0 ((p) ((a (s1 s2)))))) |}]

let%expect_test "relational: empty model" =
  let parsed = Model.parse_relational_model "[]" in
  print_s [%sexp (parsed : Model.Ast.Relational_ast.t)];
  [%expect {| () |}]

let%expect_test "relational: multiple states" =
  let parsed =
    Model.parse_relational_model "[s0: ({p, q}, [a: {s1}]), s1: ({r}, [])]"
  in
  print_s [%sexp (parsed : Model.Ast.Relational_ast.t)];
  [%expect {| ((s0 ((p q) ((a (s1))))) (s1 ((r) ()))) |}]

let%expect_test "relational: multiple actions" =
  let parsed =
    Model.parse_relational_model "[s0: ({p}, [a: {s1}, b: {s2, s3}])]"
  in
  print_s [%sexp (parsed : Model.Ast.Relational_ast.t)];
  [%expect {| ((s0 ((p) ((a (s1)) (b (s2 s3)))))) |}]

let%expect_test "relational: empty transitions" =
  let parsed = Model.parse_relational_model "[s0: ({p}, [])]" in
  print_s [%sexp (parsed : Model.Ast.Relational_ast.t)];
  [%expect {| ((s0 ((p) ()))) |}]

(* ============================================================================ *)
(* Graded Models *)
(* ============================================================================ *)

let%expect_test "graded: basic model" =
  let parsed = Model.parse_graded_model "[s0: ({p}, [a: [s1: 2, s2: 3]])]" in
  print_s [%sexp (parsed : Model.Ast.Graded_ast.t)];
  [%expect {| ((s0 ((p) ((a ((s1 2) (s2 3))))))) |}]

let%expect_test "graded: empty model" =
  let parsed = Model.parse_graded_model "[]" in
  print_s [%sexp (parsed : Model.Ast.Graded_ast.t)];
  [%expect {| () |}]

let%expect_test "graded: single grade" =
  let parsed = Model.parse_graded_model "[s0: ({p}, [a: [s1: 5]])]" in
  print_s [%sexp (parsed : Model.Ast.Graded_ast.t)];
  [%expect {| ((s0 ((p) ((a ((s1 5))))))) |}]

let%expect_test "graded: multiple actions" =
  let parsed =
    Model.parse_graded_model "[s0: ({p}, [a: [s1: 1], b: [s2: 2]])]"
  in
  print_s [%sexp (parsed : Model.Ast.Graded_ast.t)];
  [%expect {| ((s0 ((p) ((a ((s1 1))) (b ((s2 2))))))) |}]

(* ============================================================================ *)
(* Probabilistic Models *)
(* ============================================================================ *)

let%expect_test "probabilistic: basic model" =
  let parsed =
    Model.parse_probabilistic_model "[s0: ({p}, [a: [s1: 1/2, s2: 1/2]])]"
  in
  print_s [%sexp (parsed : Model.Ast.Probabilistic_ast.t)];
  [%expect {| ((s0 ((p) ((a ((s1 (1 2)) (s2 (1 2)))))))) |}]

let%expect_test "probabilistic: empty model" =
  let parsed = Model.parse_probabilistic_model "[]" in
  print_s [%sexp (parsed : Model.Ast.Probabilistic_ast.t)];
  [%expect {| () |}]

let%expect_test "probabilistic: single transition" =
  let parsed = Model.parse_probabilistic_model "[s0: ({p}, [a: [s1: 1/1]])]" in
  print_s [%sexp (parsed : Model.Ast.Probabilistic_ast.t)];
  [%expect {| ((s0 ((p) ((a ((s1 (1 1)))))))) |}]

let%expect_test "probabilistic: multiple actions" =
  let parsed =
    Model.parse_probabilistic_model "[s0: ({p}, [a: [s1: 3/4], b: [s2: 1/3]])]"
  in
  print_s [%sexp (parsed : Model.Ast.Probabilistic_ast.t)];
  [%expect {| ((s0 ((p) ((a ((s1 (3 4)))) (b ((s2 (1 3)))))))) |}]

(* ============================================================================ *)
(* Monotone Models *)
(* ============================================================================ *)

let%expect_test "monotone: basic model" =
  let parsed =
    Model.parse_monotone_model "[s0: ({p}, [a: {{s1, s2}, {s3}}])]"
  in
  print_s [%sexp (parsed : Model.Ast.Monotone_ast.t)];
  [%expect {| ((s0 ((p) ((a ((s1 s2) (s3))))))) |}]

let%expect_test "monotone: empty model" =
  let parsed = Model.parse_monotone_model "[]" in
  print_s [%sexp (parsed : Model.Ast.Monotone_ast.t)];
  [%expect {| () |}]

let%expect_test "monotone: single set" =
  let parsed = Model.parse_monotone_model "[s0: ({p}, [a: {{s1}}])]" in
  print_s [%sexp (parsed : Model.Ast.Monotone_ast.t)];
  [%expect {| ((s0 ((p) ((a ((s1))))))) |}]

let%expect_test "monotone: multiple actions" =
  let parsed =
    Model.parse_monotone_model "[s0: ({p}, [a: {{s1}}, b: {{s2}, {s3}}])]"
  in
  print_s [%sexp (parsed : Model.Ast.Monotone_ast.t)];
  [%expect {| ((s0 ((p) ((a ((s1))) (b ((s2) (s3))))))) |}]
