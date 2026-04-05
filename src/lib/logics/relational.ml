open! Core
open Naive_modcheck_coalg_common

module Relational_formula = Formula_intf.Make (struct
  open Naive_modcheck_coalg_parsers.Formula.Ast

  type 'a t = 'a relational_modality =
    | Diamond of 'a
    | Box of 'a
  [@@deriving sexp]

  let to_string f ~to_string_children =
    match f with
    | Diamond subf -> "<>" ^ to_string_children subf
    | Box subf -> "[]" ^ to_string_children subf

  let map f = function
    | Diamond x -> Diamond (f x)
    | Box x -> Box (f x)

  let negate f = function
    | Diamond x -> Box (f x)
    | Box x -> Diamond (f x)
end)

module Relational_model = Model_intf.Make (struct
  type t = State.t list [@@deriving sexp]

  let to_string states =
    let inner =
      String.concat ~sep:", "
        (List.map ~f:State.to_string states)
    in
    {%string|{%{inner}}|}
end)

module M :
  Logic_intf.LOGIC_SPECIFICATION
    with type 'a modality = 'a Relational_formula.modality
     and type transition = State.t list
     and module Model = Relational_model
     and module Formula = Relational_formula
     and module Formula_ast = Naive_modcheck_coalg_parsers
                              .Formula
                              .Ast
                              .Relational_ast = struct
  type 'a modality = 'a Relational_formula.modality
  [@@deriving sexp]

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

  let one_step_game ~model ~state
      ~(modal_formula : Formula.t Formula.modality) =
    let player =
      match modal_formula with
      | Diamond _ -> Game.Player.Eloise
      | Box _ -> Game.Player.Abelard
    in
    let subformula =
      match modal_formula with
      | Diamond f
      | Box f ->
          f
    in
    let game = Hashtbl.Poly.create () in
    let start_node =
      Game.FormulaNode (Formula.Modal modal_formula, state)
    in
    let successors =
      next_states model state (Action.of_string "")
    in
    let middle_node =
      Game.ModalNode
        (Formula.Modal modal_formula, successors)
    in
    let game_exit_nodes =
      List.map successors ~f:(fun s ->
          Game.FormulaNode (subformula, s))
    in
    Hashtbl.set game ~key:start_node
      ~data:(Game.Player.Eloise, 0, [ middle_node ]);
    Hashtbl.set game ~key:middle_node
      ~data:(player, 0, game_exit_nodes);
    let exit_nodes =
      List.map successors ~f:(fun s -> (subformula, s))
    in
    { Game.game; exit_nodes }

  let parse_formula =
    Naive_modcheck_coalg_parsers.Formula
    .parse_relational_formula

  let parse_model =
    Naive_modcheck_coalg_parsers.Model
    .parse_relational_model
end

include Logic.Make (M)
