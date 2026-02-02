open! Core
open Naive_modcheck_coalg_parsers.Model
open Naive_modcheck_coalg_parsers.Formula
open Naive_modcheck_coalg_common
open Naive_modcheck_coalg_checker
open Naive_modcheck_coalg_logics

let () =
  let model =
    parse_relational_model "[x:({p1},[{} : {x, y}]), y:({p2},[{} : {x, y}])]"
  in

  (* Test the model checker *)
  let formula_ast =
    parse_relational_formula "ν x2.(μ x1 .((p1 & <> x1) | (p2 & [] x2)) )"
  in
  let formula = Logics.Relational.formula_of_ast formula_ast in
  let result =
    Checkers.Relational.model_check ~model ~point:(State.of_string "x") ~formula
  in
  printf "Model check result: %b\n" result
