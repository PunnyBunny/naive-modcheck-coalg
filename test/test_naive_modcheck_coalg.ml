open! Core
open Naive_modcheck_coalg_parsers.Model

(* ============================================================================ *)
(* Relational Models *)
(* ============================================================================ *)

let%expect_test "relational: basic model" =
  let parsed = parse_relational_model "[s0: ({p}, [a: {s1, s2}])]" in
  print_s [%sexp (parsed : Relational_ast.t)];
  [%expect {| ((s0 ((p) ((a (s1 s2)))))) |}]

let%expect_test "relational: empty model" =
  let parsed = parse_relational_model "[]" in
  print_s [%sexp (parsed : Relational_ast.t)];
  [%expect {| () |}]

let%expect_test "relational: multiple states" =
  let parsed =
    parse_relational_model "[s0: ({p, q}, [a: {s1}]), s1: ({r}, [])]"
  in
  print_s [%sexp (parsed : Relational_ast.t)];
  [%expect {| ((s0 ((p q) ((a (s1))))) (s1 ((r) ()))) |}]

let%expect_test "relational: multiple actions" =
  let parsed = parse_relational_model "[s0: ({p}, [a: {s1}, b: {s2, s3}])]" in
  print_s [%sexp (parsed : Relational_ast.t)];
  [%expect {| ((s0 ((p) ((a (s1)) (b (s2 s3)))))) |}]

let%expect_test "relational: empty transitions" =
  let parsed = parse_relational_model "[s0: ({p}, [])]" in
  print_s [%sexp (parsed : Relational_ast.t)];
  [%expect {| ((s0 ((p) ()))) |}]

(* ============================================================================ *)
(* Graded Models *)
(* ============================================================================ *)

let%expect_test "graded: basic model" =
  let parsed = parse_graded_model "[s0: ({p}, [a: [s1: 2, s2: 3]])]" in
  print_s [%sexp (parsed : Graded_ast.t)];
  [%expect {| ((s0 ((p) ((a ((s1 2) (s2 3))))))) |}]

let%expect_test "graded: empty model" =
  let parsed = parse_graded_model "[]" in
  print_s [%sexp (parsed : Graded_ast.t)];
  [%expect {| () |}]

let%expect_test "graded: single grade" =
  let parsed = parse_graded_model "[s0: ({p}, [a: [s1: 5]])]" in
  print_s [%sexp (parsed : Graded_ast.t)];
  [%expect {| ((s0 ((p) ((a ((s1 5))))))) |}]

let%expect_test "graded: multiple actions" =
  let parsed = parse_graded_model "[s0: ({p}, [a: [s1: 1], b: [s2: 2]])]" in
  print_s [%sexp (parsed : Graded_ast.t)];
  [%expect {| ((s0 ((p) ((a ((s1 1))) (b ((s2 2))))))) |}]

(* ============================================================================ *)
(* Probabilistic Models *)
(* ============================================================================ *)

let%expect_test "probabilistic: basic model" =
  let parsed =
    parse_probabilistic_model "[s0: ({p}, [a: [s1: 1/2, s2: 1/2]])]"
  in
  print_s [%sexp (parsed : Probabilistic_ast.t)];
  [%expect {| ((s0 ((p) ((a ((s1 (1 2)) (s2 (1 2)))))))) |}]

let%expect_test "probabilistic: empty model" =
  let parsed = parse_probabilistic_model "[]" in
  print_s [%sexp (parsed : Probabilistic_ast.t)];
  [%expect {| () |}]

let%expect_test "probabilistic: single transition" =
  let parsed = parse_probabilistic_model "[s0: ({p}, [a: [s1: 1/1]])]" in
  print_s [%sexp (parsed : Probabilistic_ast.t)];
  [%expect {| ((s0 ((p) ((a ((s1 (1 1)))))))) |}]

let%expect_test "probabilistic: multiple actions" =
  let parsed =
    parse_probabilistic_model "[s0: ({p}, [a: [s1: 3/4], b: [s2: 1/3]])]"
  in
  print_s [%sexp (parsed : Probabilistic_ast.t)];
  [%expect {| ((s0 ((p) ((a ((s1 (3 4)))) (b ((s2 (1 3)))))))) |}]

(* ============================================================================ *)
(* Monotone Models *)
(* ============================================================================ *)

let%expect_test "monotone: basic model" =
  let parsed = parse_monotone_model "[s0: ({p}, [a: {{s1, s2}, {s3}}])]" in
  print_s [%sexp (parsed : Monotone_ast.t)];
  [%expect {| ((s0 ((p) ((a ((s1 s2) (s3))))))) |}]

let%expect_test "monotone: empty model" =
  let parsed = parse_monotone_model "[]" in
  print_s [%sexp (parsed : Monotone_ast.t)];
  [%expect {| () |}]

let%expect_test "monotone: single set" =
  let parsed = parse_monotone_model "[s0: ({p}, [a: {{s1}}])]" in
  print_s [%sexp (parsed : Monotone_ast.t)];
  [%expect {| ((s0 ((p) ((a ((s1))))))) |}]

let%expect_test "monotone: multiple actions" =
  let parsed =
    parse_monotone_model "[s0: ({p}, [a: {{s1}}, b: {{s2}, {s3}}])]"
  in
  print_s [%sexp (parsed : Monotone_ast.t)];
  [%expect {| ((s0 ((p) ((a ((s1))) (b ((s2) (s3))))))) |}]
