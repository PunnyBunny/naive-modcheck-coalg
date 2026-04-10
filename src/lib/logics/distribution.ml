open! Core
open Naive_modcheck_coalg_common
open Naive_modcheck_coalg_parsers_model
open Naive_modcheck_coalg_parsers_formula

module Make (M : sig
  type formula
  type state
end) =
struct
  module Model_spec = Model_parsers.Distribution
  module Formula_spec = Formula_parsers.Distribution

  type inner_node = M.state list

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
      ~transition:(dist : M.state Model_spec.t)
      ~(modal_formula : M.formula Formula_spec.t) :
      one_step_game_t =
    (* TODO: see if can impl Hausmann 2019 *)
    let open Frac in
    let formula =
      match modal_formula with
      | GT (_, f)
      | GE (_, f)
      | LT (_, f)
      | LE (_, f) ->
          f
    in
    let game = Hashtbl.Poly.create () in
    let get_prob state = Hashtbl.find_exn dist state in
    let rec get_eloise_nodes_list xs total_prob states =
      match states with
      | [] -> (
          match modal_formula with
          | GT (p, _) when total_prob > p -> [ xs ]
          | GE (p, _) when total_prob >= p -> [ xs ]
          | _ -> [])
      | s :: ss ->
          get_eloise_nodes_list xs total_prob ss
          @ get_eloise_nodes_list (s :: xs)
              (total_prob + get_prob s)
              ss
    in
    let eloise_nodes_list =
      get_eloise_nodes_list [] Frac.zero (Hashtbl.keys dist)
    in
    Hashtbl.set game ~key:Start
      ~data:
        ( Game.Player.Eloise
        , 0
        , List.map eloise_nodes_list ~f:(fun xs -> Inner xs)
        );
    List.iter eloise_nodes_list ~f:(fun xs ->
        Hashtbl.set game ~key:(Inner xs)
          ~data:
            ( Game.Player.Abelard
            , 0
            , List.map xs ~f:(fun x -> Exit (formula, x)) ));
    game
end
