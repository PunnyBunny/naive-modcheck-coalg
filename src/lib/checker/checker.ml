open! Core
open Naive_modcheck_coalg_common
open Naive_modcheck_coalg_logics

module Make (L : Logic.S) :
  Checker_intf.S with module Logic = L = struct
  module Logic = L

  type node = (Logic.Formula.t, State.t) Game.node
  [@@deriving sexp]

  type game =
    ( node
    , Game.Player.t * Game.Priority.t * node list )
    Hashtbl.Poly.t
  [@@deriving sexp]

  let pretty_print_formula node =
    match node with
    | Game.FormulaNode (formula, _)
    | Game.ModalNode (formula, _) ->
        Logic.Formula.pretty_print formula

  (* Build parity game from model and formula *)
  let build_game ~(model : Logic.Model.t) ~(point : State.t)
      ~(formula : Logic.Formula.t) : game =
    let game = Hashtbl.Poly.create () in
    let _states = Logic.get_states ~model in
    let { theta; alternation_depth } :
        Logic.helper_functions =
      Logic.get_helper_functions formula
    in

    let rec build_formula_node (formula : Logic.Formula.t)
        (state : State.t) : unit =
      let node = Game.FormulaNode (formula, state) in
      if Hashtbl.mem game node then ()
      else
        let add_node (owner : Game.Player.t) priority
            successors =
          Hashtbl.set game ~key:node
            ~data:(owner, priority, successors)
        in
        match formula with
        | True -> add_node Abelard 0 []
        | False -> add_node Eloise 0 []
        | Ap atom ->
            let has_atom =
              Logic.is_atom_in_state ~model ~state ~atom
            in
            add_node
              (if has_atom then Abelard else Eloise)
              0 []
        | Not atom ->
            let has_atom =
              Logic.is_atom_in_state ~model ~state ~atom
            in
            add_node
              (if has_atom then Eloise else Abelard)
              0 []
        | And (sub_fmla1, sub_fmla2) ->
            add_node Abelard 0
              [
                Game.FormulaNode (sub_fmla1, state)
              ; Game.FormulaNode (sub_fmla2, state)
              ];
            build_formula_node sub_fmla1 state;
            build_formula_node sub_fmla2 state
        | Or (sub_fmla1, sub_fmla2) ->
            add_node Eloise 0
              [
                Game.FormulaNode (sub_fmla1, state)
              ; Game.FormulaNode (sub_fmla2, state)
              ];
            build_formula_node sub_fmla1 state;
            build_formula_node sub_fmla2 state
        | Mu (_x, sub_fmla)
        | Nu (_x, sub_fmla) ->
            add_node Eloise 0
              [ Game.FormulaNode (sub_fmla, state) ];
            build_formula_node sub_fmla state
        | Var x -> (
            match theta x with
            | Some f_def ->
                let priority =
                  alternation_depth x |> Option.value_exn
                in
                add_node Eloise priority
                  [ Game.FormulaNode (f_def, state) ];
                build_formula_node f_def state
            | None ->
                failwith
                  ("Unbound variable: " ^ Var.to_string x))
        | Modal modal_fmla ->
            let { Game.game = one_step_game; exit_nodes } =
              Logic.one_step_game ~model ~state
                ~modal_formula:modal_fmla
            in
            Hashtbl.iteri one_step_game
              ~f:(fun
                  ~key:game_node
                  ~data:(owner, priority, successors)
                ->
                Hashtbl.set game ~key:game_node
                  ~data:(owner, priority, successors));
            let initial_node =
              Game.FormulaNode (formula, state)
            in
            if not (Hashtbl.mem game initial_node) then
              failwith
                "One-step game did not populate initial \
                 node";
            List.iter exit_nodes
              ~f:(fun (sub_fmla, s) ->
                build_formula_node sub_fmla s)
    in

    build_formula_node formula point;
    game

  (* Solve the parity game using PGSolver, returning full game data *)
  let solve_game_internal ~verbose (game : game)
      (starting_node : node) : Checker_intf.game_data =
    let node_keys = Hashtbl.keys game |> List.of_list in
    let node_key_array = Array.of_list node_keys in
    let n = Array.length node_key_array in

    let key_to_index =
      List.mapi node_keys ~f:(fun i node -> (node, i))
      |> Hashtbl.Poly.of_alist_exn
    in

    let pgsolver_parity_game =
      Paritygame.pg_init n (fun i ->
          let node = node_key_array.(i) in
          let owner, priority, successors =
            Hashtbl.find_exn game node
          in

          let successor_indices =
            List.map successors ~f:(fun succ ->
                Hashtbl.find_exn key_to_index succ)
          in

          let owner_idx =
            match owner with
            | Game.Player.Eloise -> Paritygame.plr_Even
            | Game.Player.Abelard -> Paritygame.plr_Odd
          in

          ( priority
          , owner_idx
          , successor_indices
          , Some (sexp_of_node node |> Sexp.to_string) ))
    in

    let solution, strategy =
      Recursive.solve pgsolver_parity_game
    in
    let starting_index =
      Hashtbl.find_exn key_to_index starting_node
    in
    let winner = solution.(starting_index) in

    if verbose then (
      printf "[Game]\n%s\n"
        (Paritygame.game_to_string pgsolver_parity_game);
      printf "[Solution]\n%s\n\n"
        (Paritygame.format_solution solution);
      printf "[Strategy]\n%s\n\n"
        (Paritygame.format_strategy strategy);
      printf "\n[Winner]: %s\n%!"
        (if Poly.(winner = Paritygame.plr_Even) then
           "Eloise"
         else "Abelard"));

    let result = Poly.(winner = Paritygame.plr_Even) in
    let node_labels =
      Array.map node_key_array ~f:pretty_print_formula
    in
    let node_owners =
      Array.map node_key_array ~f:(fun node ->
          let owner, _, _ = Hashtbl.find_exn game node in
          match owner with
          | Game.Player.Eloise -> "Eloise"
          | Game.Player.Abelard -> "Abelard")
    in
    let node_priorities =
      Array.map node_key_array ~f:(fun node ->
          let _, priority, _ = Hashtbl.find_exn game node in
          priority)
    in
    let node_successors =
      Array.init n ~f:(fun i ->
          let node = node_key_array.(i) in
          let _, _, succs = Hashtbl.find_exn game node in
          List.map succs ~f:(fun s ->
              Hashtbl.find_exn key_to_index s))
    in
    let winners =
      Array.map solution ~f:(fun plr ->
          if Poly.(plr = Paritygame.plr_Even) then "Eloise"
          else "Abelard")
    in
    let strategy_arr =
      Array.map strategy ~f:(fun s ->
          if s >= 0 then Some s else None)
    in
    let node_is_modal =
      Array.map node_key_array ~f:(fun node ->
          match node with
          | Game.ModalNode _ -> true
          | _ -> false)
    in
    let node_states =
      Array.map node_key_array ~f:(fun node ->
          match node with
          | Game.FormulaNode (_, state) ->
              [ State.to_string state ]
          | Game.ModalNode (_, states) ->
              List.map states ~f:State.to_string)
    in
    {
      Checker_intf.num_nodes = n
    ; node_labels
    ; node_owners
    ; node_priorities
    ; node_successors
    ; winners
    ; strategy = strategy_arr
    ; node_is_modal
    ; node_states
    ; starting_node = starting_index
    ; result
    }

  (* Main model checking entry point — returns full game data *)
  let model_check_full ~verbose ~(model : Logic.Model.t)
      ~(point : State.t) ~(formula : Logic.Formula.t) :
      Checker_intf.game_data =
    let game = build_game ~model ~point ~formula in
    let starting_node = Game.FormulaNode (formula, point) in
    solve_game_internal ~verbose game starting_node

  (* Main model checking entry point — returns bool *)
  let model_check ~verbose ~(model : Logic.Model.t)
      ~(point : State.t) ~(formula : Logic.Formula.t) : bool
      =
    (model_check_full ~verbose ~model ~point ~formula)
      .result
end
