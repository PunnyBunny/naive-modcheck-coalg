open! Core
open Naive_modcheck_coalg_common
open Naive_modcheck_coalg_parsers_model
open Naive_modcheck_coalg_parsers_formula

module type EXP_BY_SET_SPEC = sig
  type t [@@deriving sexp, compare, hash, to_string]

  val parser : t Angstrom.t
end

module Make
    (A : EXP_BY_SET_SPEC)
    (S : Logic_intf.LOGIC_SPECIFICATION)
    (M : sig
      type formula
      type state
    end) =
struct
  module S = S (M)

  module S_closed =
    Formula_parsers.Propositional_closure.Make
      (S.Formula_spec)

  module Model_spec =
    Model_parsers.Exp_by_set.Make (A) (S.Model_spec)

  module Formula_spec =
    Formula_parsers.Exp_by_set.Make (A) (S.Formula_spec)

  type inner_node =
    | A of M.formula S_closed.t
    | S_inner of S.inner_node

  type one_step_node =
    | Start
    | Inner of inner_node
    | Exit of M.formula * M.state

  type one_step_game_t =
    ( one_step_node
    , Game.Player.t * Game.Priority.t * one_step_node list
    )
    Hashtbl.Poly.t

  let one_step_satisfaction ~model:_ ~box_or_diamond:_
      ~state:_ ~states:_ ~action:_ =
    (* Placeholder: will be implemented with actual game logic *)
    true

  let one_step_game ~transition:(tbl : M.state Model_spec.t)
      ~(modal_formula : M.formula Formula_spec.t) :
      one_step_game_t =
    let game = Hashtbl.Poly.create () in
    let a, formula = modal_formula in
    let add_edge src_node dst_nodes player =
      Hashtbl.update game src_node ~f:(function
        | None -> (player, 0, dst_nodes)
        | Some (player', priority, succs) ->
            if Game.Player.equal player' player then
              (player', priority, dst_nodes @ succs)
            else failwith "Player mismatch when adding edge")
    in
    let convert_s_node (formula : M.formula S_closed.t)
        (node : S.one_step_node) =
      match node with
      | S.Start -> Inner (A formula)
      | S.Inner inner -> Inner (S_inner inner)
      | S.Exit (formula, state) -> Exit (formula, state)
    in
    let add_game formula (game : S.one_step_game_t) =
      Hashtbl.iteri game
        ~f:(fun ~key:node ~data:(player, _, succs) ->
          let new_key = convert_s_node formula node in
          let new_succs =
            List.map succs ~f:(convert_s_node formula)
          in
          add_edge new_key new_succs player)
    in
    let rec add_games (formula : M.formula S_closed.t) =
      match formula with
      | True
      | False ->
          ()
      | And (f1, f2) ->
          add_games f1;
          add_games f2;
          add_edge (Inner (A formula))
            [ Inner (A f1); Inner (A f2) ]
            Game.Player.Abelard
      | Or (f1, f2) ->
          add_games f1;
          add_games f2;
          add_edge (Inner (A formula))
            [ Inner (A f1); Inner (A f2) ]
            Game.Player.Eloise
      | Modal body ->
          let s_game =
            S.one_step_game
              ~transition:(Hashtbl.find_exn tbl a)
              ~modal_formula:body
          in
          add_game formula s_game
    in
    add_games formula;
    game
end
