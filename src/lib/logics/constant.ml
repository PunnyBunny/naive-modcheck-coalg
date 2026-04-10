open! Core
open Naive_modcheck_coalg_common
open Naive_modcheck_coalg_parsers_model
open Naive_modcheck_coalg_parsers_formula

module type CONSTANT_SPEC = sig
  type t [@@deriving sexp, to_string, equal]

  val parser : t Angstrom.t
end

module Make
    (A : CONSTANT_SPEC)
    (M : sig
      type formula
      type state
    end) =
struct
  module Model_spec = Model_parsers.Constant.Make (A)
  module Formula_spec = Formula_parsers.Constant.Make (A)

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
      ~transition:(curr : M.state Model_spec.t)
      ~(modal_formula : M.formula Formula_spec.t) :
      one_step_game_t =
    let game = Hashtbl.Poly.create () in
    let owner =
      match modal_formula with
      | Normal a when A.equal a curr -> Game.Player.Abelard
      | Not a when not (A.equal a curr) ->
          Game.Player.Abelard
      | _ -> Game.Player.Eloise
    in
    Hashtbl.set game ~key:Start ~data:(owner, 0, []);
    game
end
