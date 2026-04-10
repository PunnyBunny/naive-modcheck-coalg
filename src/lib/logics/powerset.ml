open! Core
open Naive_modcheck_coalg_common
open Naive_modcheck_coalg_parsers_model
open Naive_modcheck_coalg_parsers_formula

module Make (M : sig
  type formula
  type state
end) =
struct
  module Model_spec = Model_parsers.Powerset
  module Formula_spec = Formula_parsers.Powerset

  type inner_node

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

  let one_step_game
      ~transition:(succs : M.state Model_spec.t)
      ~(modal_formula : M.formula Formula_spec.t) :
      one_step_game_t =
    let game = Hashtbl.Poly.create () in
    let formula, owner =
      match modal_formula with
      | Box f -> (f, Game.Player.Abelard)
      | Diamond f -> (f, Game.Player.Eloise)
    in
    let exit_nodes =
      List.map succs ~f:(fun s -> Exit (formula, s))
    in
    Hashtbl.set game ~key:Start ~data:(owner, 0, exit_nodes);
    game
end
