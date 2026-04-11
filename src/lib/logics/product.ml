open! Core
open Naive_modcheck_coalg_common
open Naive_modcheck_coalg_parsers_model
open Naive_modcheck_coalg_parsers_formula

module Make
    (A : Logic_intf.LOGIC_SPECIFICATION)
    (B : Logic_intf.LOGIC_SPECIFICATION) =
struct
  module A_closed =
    Formula_parsers.Propositional_closure.Make
      (A.Formula_spec)

  module B_closed =
    Formula_parsers.Propositional_closure.Make
      (B.Formula_spec)

  module Model_spec =
    Model_parsers.Product.Make (A.Model_spec) (B.Model_spec)

  module Formula_spec =
    Formula_parsers.Product.Make
      (A.Formula_spec)
      (B.Formula_spec)

  module One_step (M : sig
    type formula [@@deriving sexp]
    type state [@@deriving sexp]
  end) =
  struct
    module A_inst = A.One_step (M)
    module B_inst = B.One_step (M)

    type inner_node =
      | A_formula of M.formula A_closed.t
      | B_formula of M.formula B_closed.t
      | A_inner of A_inst.inner_node (* Inner nodes of A *)
      | B_inner of B_inst.inner_node (* Inner nodes of B *)
    [@@deriving sexp]

    type one_step_node =
      | Start
      | Inner of inner_node
      | Exit of M.formula * M.state
    [@@deriving sexp]

    type one_step_game_t =
      ( one_step_node
      , Game.Player.t * Game.Priority.t * one_step_node list
      )
      Hashtbl.Poly.t
    [@@deriving sexp]

    let one_step_game ~(transition : M.state Model_spec.t)
        ~(modal_formula : M.formula Formula_spec.t) :
        one_step_game_t =
      let game = Hashtbl.Poly.create () in
      let a_transition, b_transition = transition in
      let convert_a_node a_formula
          (a_node : A_inst.one_step_node) =
        match a_node with
        | Start -> Inner (A_formula a_formula)
        | Inner inner -> Inner (A_inner inner)
        | Exit (formula, state) -> Exit (formula, state)
      in
      let convert_b_node b_formula
          (b_node : B_inst.one_step_node) =
        match b_node with
        | Start -> Inner (B_formula b_formula)
        | Inner inner -> Inner (B_inner inner)
        | Exit (formula, state) -> Exit (formula, state)
      in
      let add_a_game (a_formula : M.formula A_closed.t)
          (a_game : A_inst.one_step_game_t) =
        Hashtbl.iteri a_game
          ~f:(fun
              ~key:node ~data:(player, priority, succs) ->
            let new_key = convert_a_node a_formula node in
            let new_succs =
              List.map succs ~f:(convert_a_node a_formula)
            in
            Hashtbl.set game ~key:new_key
              ~data:(player, priority, new_succs))
      in
      let add_b_game (b_formula : M.formula B_closed.t)
          (b_game : B_inst.one_step_game_t) =
        Hashtbl.iteri b_game
          ~f:(fun
              ~key:node ~data:(player, priority, succs) ->
            let new_key = convert_b_node b_formula node in
            let new_succs =
              List.map succs ~f:(convert_b_node b_formula)
            in
            Hashtbl.set game ~key:new_key
              ~data:(player, priority, new_succs))
      in
      let add_edge src_node dst_nodes player =
        Hashtbl.update game src_node ~f:(function
          | None -> (player, 0, dst_nodes)
          | Some (player', priority, succs) ->
              if Game.Player.equal player' player then
                (player', priority, dst_nodes @ succs)
              else
                failwith "Player mismatch when adding edge")
      in
      let rec add_a_games
          (a_formula_closed : M.formula A_closed.t) =
        match a_formula_closed with
        | True
        | False ->
            ()
        | And (a, b) ->
            add_a_games a;
            add_a_games b;
            add_edge (Inner (A_formula a_formula_closed))
              [ Inner (A_formula a); Inner (A_formula b) ]
              Game.Player.Abelard
        | Or (a, b) ->
            add_a_games a;
            add_a_games b;
            add_edge (Inner (A_formula a_formula_closed))
              [ Inner (A_formula a); Inner (A_formula b) ]
              Game.Player.Eloise
        | Modal body ->
            let a_game =
              A_inst.one_step_game ~transition:a_transition
                ~modal_formula:body
            in
            add_a_game (Modal body) a_game
      in
      let rec add_b_games
          (b_formula_closed : M.formula B_closed.t) =
        match b_formula_closed with
        | True
        | False ->
            ()
        | And (a, b) ->
            add_b_games a;
            add_b_games b;
            add_edge (Inner (B_formula b_formula_closed))
              [ Inner (B_formula a); Inner (B_formula b) ]
              Game.Player.Abelard
        | Or (a, b) ->
            add_b_games a;
            add_b_games b;
            add_edge (Inner (B_formula b_formula_closed))
              [ Inner (B_formula a); Inner (B_formula b) ]
              Game.Player.Eloise
        | Modal body ->
            let b_game =
              B_inst.one_step_game ~transition:b_transition
                ~modal_formula:body
            in
            add_b_game (Modal body) b_game
      in
      (match modal_formula with
      | A a_formula ->
          add_a_games a_formula;
          add_edge Start
            [ Inner (A_formula a_formula) ]
            Game.Player.Eloise
      | B b_formula ->
          add_b_games b_formula;
          add_edge Start
            [ Inner (B_formula b_formula) ]
            Game.Player.Eloise);
      game
  end
end
