open! Core
open Naive_modcheck_coalg_common

module Relational_formula = Formula_intf.Make (struct
  type t = unit [@@deriving sexp]

  let to_string () = ""
end)

module Relational_model = Model_intf.Make (struct
  type t = State.t list [@@deriving sexp]

  let to_string states =
    let inner = String.concat ~sep:", " (List.map ~f:State.to_string states) in
    {%string|{%{inner}}|}
end)

module M :
  Logic_intf.LOGIC_SPECIFICATION
    with type modality = unit
     and type transition = State.t list
     and module Model = Relational_model
     and module Formula = Relational_formula
     and module Formula_ast = Naive_modcheck_coalg_parsers
                              .Formula
                              .Ast
                              .Relational_ast = struct
  type modality = unit [@@deriving sexp]
  type transition = State.t list [@@deriving sexp]

  module Model = Relational_model
  module Formula = Relational_formula

  module Formula_ast =
    Naive_modcheck_coalg_parsers.Formula.Ast.Relational_ast

  (* TODO: move into interface *)
  let next_states model state action =
    match Hashtbl.find model state with
    | None -> []
    | Some (_, transitions) ->
        if Action.is_empty action then
          Hashtbl.data transitions |> List.concat
        else
          Hashtbl.find transitions action
          |> Option.value_exn ~message:"No transition found"

  let one_step_satisfaction ~(model : Model.t)
      ~(box_or_diamond : [ `Box | `Diamond ])
      ~(state : State.t) ~(states : State.t list)
      ~(action : Action.t) =
    let successors = next_states model state action in
    match box_or_diamond with
    | `Diamond ->
        List.exists successors ~f:(fun s ->
            List.mem states s ~equal:State.equal)
    | `Box ->
        List.for_all successors ~f:(fun s ->
            List.mem states s ~equal:State.equal)

  let parse_formula =
    Naive_modcheck_coalg_parsers.Formula
    .parse_relational_formula

  let parse_model =
    Naive_modcheck_coalg_parsers.Model
    .parse_relational_model
end

include Logic.Make (M)
