open! Core
open Naive_modcheck_coalg_common
open Naive_modcheck_coalg_parsers_model
open Naive_modcheck_coalg_parsers_formula

module Make
    (A : Logic_intf.LOGIC_SPECIFICATION)
    (B : Logic_intf.LOGIC_SPECIFICATION) =
struct
  module B_closed =
    Formula_parsers.Propositional_closure.Make
      (B.Formula_spec)

  module Model_spec =
    Model_parsers.Composition.Make
      (A.Model_spec)
      (B.Model_spec)

  module Formula_spec =
    Formula_parsers.Composition.Make
      (A.Formula_spec)
      (B.Formula_spec)

  module One_step (M : sig
    type formula [@@deriving sexp]
    type state [@@deriving sexp]
  end) =
  struct
    module B_inst = B.One_step (M)

    module A_inst = A.One_step (struct
      type formula = M.formula B_closed.t [@@deriving sexp]
      type state = M.state B.Model_spec.t [@@deriving sexp]
    end)

    type inner_node =
      | A_inner of A_inst.inner_node (* Inner nodes of A *)
      | Both of
          M.formula B_closed.t * M.state B.Model_spec.t
        (* Exit nodes of A = Start nodes of B *)
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
      let a_game =
        A_inst.one_step_game ~transition ~modal_formula
      in
      let convert_a_node (node : A_inst.one_step_node) =
        match node with
        | Start -> Start
        | Inner x -> Inner (A_inner x)
        | Exit (f, s) -> Inner (Both (f, s))
      in
      let game =
        let result = Hashtbl.Poly.create () in
        Hashtbl.iteri a_game ~f:(fun ~key ~data ->
            let new_key = convert_a_node key in
            let player, priority, succs = data in
            let new_succs =
              List.map succs ~f:convert_a_node
            in
            Hashtbl.set result ~key:new_key
              ~data:(player, priority, new_succs));
        result
      in
      let b_transitions =
        (* To get useful functor elements of type
           B(X) out of A's game A(B(X)) *)
        Hashtbl.keys a_game
        |> List.filter_map ~f:(function
          | Exit (_, s) -> Some s
          | _ -> None)
      in
      (* For every exit node, create a corresponding game for B *)
      let convert_b_node (b_formula : M.formula B_closed.t)
          (b_transition : M.state B.Model_spec.t)
          (node : B_inst.one_step_node) =
        match node with
        | Start -> Inner (Both (b_formula, b_transition))
        | Inner x -> Inner (B_inner x)
        | Exit (f, s) -> Exit (f, s)
      in
      let add_b_game (b_formula : M.formula B_closed.t)
          (b_transition : M.state B.Model_spec.t)
          (b_game : B_inst.one_step_game_t) =
        Hashtbl.iteri b_game ~f:(fun ~key ~data ->
            let player, priority, succs = data in
            let new_key =
              convert_b_node b_formula b_transition key
            in
            let new_succs =
              List.map succs
                ~f:(convert_b_node b_formula b_transition)
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
      List.iter b_transitions ~f:(fun b_transition ->
          let rec add_b_games
              (formula : M.formula B_closed.t) =
            match formula with
            | True
            | False ->
                ()
            | And (a, b) ->
                add_b_games a;
                add_b_games b;
                add_edge
                  (Inner (Both (formula, b_transition)))
                  [
                    Inner (Both (a, b_transition))
                  ; Inner (Both (b, b_transition))
                  ]
                  Game.Player.Abelard
            | Or (a, b) ->
                add_b_games a;
                add_b_games b;
                add_edge
                  (Inner (Both (formula, b_transition)))
                  [
                    Inner (Both (a, b_transition))
                  ; Inner (Both (b, b_transition))
                  ]
                  Game.Player.Eloise
            | Modal body ->
                let b_game =
                  B_inst.one_step_game
                    ~transition:b_transition
                    ~modal_formula:body
                in
                add_b_game formula b_transition b_game
          in
          ignore
            (A.Formula_spec.map modal_formula
               ~f:(fun inner_formula ->
                 add_b_games inner_formula)));
      game
  end
end
