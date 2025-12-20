open! Core
open Naive_modcheck_coalg_parsers.Model
open Naive_modcheck_coalg_common
open Naive_modcheck_coalg_checker
open Naive_modcheck_coalg_logics

let () =
  let parsed_model =
    parse_relational_model "[x:({p1},[{} : {x, y}]), y:({p2},[{} : {x, y}])]"
  in

  (* Test the model checker *)
  let open Logics.Relational in
  let x1 = Var.of_string "x1" in
  let x2 = Var.of_string "x2" in
  let formula =
    Nu
      ( x2,
        Mu
          ( x1,
            Or
              ( And
                  ( Ap (Ap.of_string "p1"),
                    Diamond (Action.of_string "", (), Var x1) ),
                And
                  (Ap (Ap.of_string "p2"), Box (Action.of_string "", (), Var x2))
              ) ) )
    (* ν x2.(μ x1 .((p1 & <> x1) | (p2 & [] x2))) *)
  in
  let model = model_of_ast parsed_model in
  let result =
    Checkers.Relational.model_check ~model ~point:(State.of_string "x") ~formula
  in
  printf "Model check result: %b\n" result
