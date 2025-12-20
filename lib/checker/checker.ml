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

  module Priority = struct
    type t = int [@@deriving sexp]
  end

  module Player = struct
    type t = Eloise | Abelard [@@deriving sexp]
  end

  type game = (node, Player.t * Priority.t * node list) Hashtbl.Poly.t
  [@@deriving sexp]

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
    let { theta; alternation_depth } : Logic.helper_functions =
      Logic.get_helper_functions formula
    in

    let rec build_formula_node (formula : Logic.formula) (state : State.t) :
        unit =
      let node = FormulaNode (formula, state) in
      if Hashtbl.mem game node then ()
      else
        let add_node (owner : Player.t) priority successors =
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
        | Mu (x, sub_fmla) | Nu (x, sub_fmla) ->
            let priority = alternation_depth x |> Option.value_exn in
            add_node Eloise priority [ FormulaNode (sub_fmla, state) ];
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
                  Logic.one_step_satisfaction ~box_or_diamond:`Diamond ~model
                    ~state ~states:state_set ~action:a)
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
                  Logic.one_step_satisfaction ~box_or_diamond:`Box ~model ~state
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

  (* Convert node to string for debugging *)
  let string_of_node (node : node) : string = Sexp.to_string (sexp_of_node node)

  (* Solve the parity game using PGSolver *)
  let solve_game (game : game) (starting_node : node) : bool =
    let node_keys = Hashtbl.keys game |> List.of_list in
    let node_key_array = Array.of_list node_keys in

    let key_to_index =
      List.mapi node_keys ~f:(fun i node -> (node, i))
      |> Hashtbl.Poly.of_alist_exn
    in

    let pgsolver_parity_game =
      Paritygame.pg_init (Array.length node_key_array) (fun i ->
          let node = node_key_array.(i) in
          let owner, priority, successors = Hashtbl.find_exn game node in

          let successor_indices =
            List.map successors ~f:(fun succ ->
                Hashtbl.find_exn key_to_index succ)
          in

          let owner_idx =
            match owner with
            | Eloise -> Paritygame.plr_Even
            | Abelard -> Paritygame.plr_Odd
          in

          (priority, owner_idx, successor_indices, Some (string_of_node node)))
    in

    let solver, _, _ = Solvers.find_solver "recursive" in
    let solution, strategy = solver [||] pgsolver_parity_game in
    let starting_index = Hashtbl.find_exn key_to_index starting_node in
    let winner = solution.(starting_index) in

    printf "[Game]\n%s\n" (Paritygame.game_to_string pgsolver_parity_game);
    printf "[Solution]\n%s\n\n" (Paritygame.format_solution solution);
    printf "[Strategy]\n%s\n\n" (Paritygame.format_strategy strategy);
    printf "\n[Winner]: %s\n%!"
      (if Poly.(winner = Paritygame.plr_Even) then "Eloise" else "Abelard");
    Poly.(winner = Paritygame.plr_Even)

  (* Main model checking entry point *)
  let model_check ~(model : Logic.model) ~(point : State.t)
      ~(formula : Logic.formula) : bool =
    let game = build_game ~model ~point ~formula in
    let starting_node = FormulaNode (formula, point) in

    solve_game game starting_node
end
