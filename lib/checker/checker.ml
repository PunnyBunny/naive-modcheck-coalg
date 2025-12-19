open! Core
open Naive_modcheck_coalg_common
open Naive_modcheck_coalg_logics

module Make (L : Logic.S) : Checker_intf.S with module Logic = L = struct
  module Logic = L

  (* Game node types *)
  type node =
    | FormulaNode of Logic.formula * State.t
    | ModalNode of Logic.formula * State.t list
  [@@deriving sexp]

  type priority = int
  type player = Eloise | Abelard [@@deriving sexp]
  type game = (node, player * priority * node list) Hashtbl.t

  (* Helper: powerset *)
  let rec powerset lst =
    match lst with
    | [] -> [ [] ]
    | x :: xs ->
        let ps = powerset xs in
        ps @ List.map ps ~f:(fun subset -> x :: subset)

  (* Build parity game from model and formula *)
  let build_game ~(model : Logic.model) ~(point : State.t)
      ~(formula : Logic.formula) : game =
    let game = Hashtbl.Poly.create () in
    let states = Logic.get_states ~model in
    let powerset_of_states = powerset states in
    let theta = Logic.get_theta formula in

    let rec build_formula_node (formula : Logic.formula) (state : State.t) :
        unit =
      let node = FormulaNode (formula, state) in
      if Hashtbl.mem game node then ()
      else
        let add_node owner priority successors =
          Hashtbl.set game ~key:node ~data:(owner, priority, successors)
        in
        match formula with
        | True -> add_node Abelard 0 []
        | False -> add_node Eloise 0 []
        | Ap atom ->
            let has_atom = Logic.is_atom_in_state ~model ~state ~atom in
            add_node (if has_atom then Abelard else Eloise) 0 []
        | Not atom ->
            let has_atom = Logic.is_atom_in_state ~model ~state ~atom in
            add_node (if has_atom then Eloise else Abelard) 0 []
        | And (sub_fmla1, sub_fmla2) ->
            add_node Abelard 0
              [ FormulaNode (sub_fmla1, state); FormulaNode (sub_fmla2, state) ];
            build_formula_node sub_fmla1 state;
            build_formula_node sub_fmla2 state
        | Or (sub_fmla1, sub_fmla2) ->
            add_node Eloise 0
              [ FormulaNode (sub_fmla1, state); FormulaNode (sub_fmla2, state) ];
            build_formula_node sub_fmla1 state;
            build_formula_node sub_fmla2 state
        | Mu (_x, sub_fmla) ->
            (* TODO: Mu has odd priority (approx: 1 for now) *)
            add_node Eloise 1 [ FormulaNode (sub_fmla, state) ];
            build_formula_node sub_fmla state
        | Nu (_x, sub_fmla) ->
            (* TODO: Nu has even priority (approx: 2 for now) *)
            add_node Eloise 2 [ FormulaNode (sub_fmla, state) ];
            build_formula_node sub_fmla state
        | Var x -> (
            match theta x with
            | Some f_def ->
                add_node Eloise 0 [ FormulaNode (f_def, state) ];
                build_formula_node f_def state
            | None -> failwith ("Unbound variable: " ^ Var.to_string x))
        | Diamond (a, _m, sub_fmla) ->
            let satisfying_sets =
              List.filter powerset_of_states ~f:(fun state_set ->
                  Logic.predicate_lifting ~box_or_diamond:`Diamond ~model ~state
                    ~states:state_set ~action:a)
            in
            let modal_nodes =
              List.map satisfying_sets ~f:(fun states ->
                  ModalNode (sub_fmla, states))
            in
            add_node Eloise 0 modal_nodes;
            List.iter satisfying_sets ~f:(fun states ->
                build_modal_node sub_fmla states)
        | Box (a, _m, sub_fmla) ->
            let satisfying_sets =
              List.filter powerset_of_states ~f:(fun state_set ->
                  Logic.predicate_lifting ~box_or_diamond:`Box ~model ~state
                    ~states:state_set ~action:a)
            in
            let modal_nodes =
              List.map satisfying_sets ~f:(fun states ->
                  ModalNode (sub_fmla, states))
            in
            add_node Abelard 0 modal_nodes;
            List.iter satisfying_sets ~f:(fun states ->
                build_modal_node sub_fmla states)
    and build_modal_node (sub_fmla : Logic.formula) (states : State.t list) :
        unit =
      let node = ModalNode (sub_fmla, states) in
      if Hashtbl.mem game node then ()
      else begin
        let formula_nodes =
          List.map states ~f:(fun state -> FormulaNode (sub_fmla, state))
        in
        Hashtbl.set game ~key:node ~data:(Abelard, 0, formula_nodes);
        List.iter states ~f:(fun state -> build_formula_node sub_fmla state)
      end
    in

    build_formula_node formula point;
    game

  (* Simple fixpoint solver (placeholder - would use proper parity game solver) *)
  let solve_game (game : game) (starting_node : node) : bool =
    (* For now, just check if starting node is owned by Abelard (very naive) *)
    match Hashtbl.find game starting_node with
    | None -> false
    | Some (owner, _, _) -> (
        match owner with Abelard -> true | Eloise -> false)

  (* Main model checking entry point *)
  let model_check ~(model : Logic.model) ~(point : State.t)
      ~(formula : Logic.formula) : bool =
    (* Create empty theta (no bound variables initially) *)
    let game = build_game ~model ~point ~formula in
    let starting_node = FormulaNode (formula, point) in

    solve_game game starting_node
end
