open! Core
open Naive_modcheck_coalg_parsers.Model
open Naive_modcheck_coalg_common
open Naive_modcheck_coalg_checker
open Naive_modcheck_coalg_logics

let () =
  let parsed = parse_relational_model "[s0: ({p}, [a: {s1, s2}])]" in
  print_s [%sexp (parsed : Relational_ast.t)];

  (* Test the model checker *)
  let open Logics.Relational in
  let formula =
    And (Ap (Ap.of_string "p"), Diamond (Action.of_string "a", (), True))
  in
  let model = model_of_ast parsed in
  let result =
    Checkers.Relational.model_check ~model ~point:(State.of_string "s0")
      ~formula
  in
  printf "Model check result: %b\n" result
