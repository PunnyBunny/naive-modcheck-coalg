open! Core
open Naive_modcheck_coalg_common
open Naive_modcheck_coalg_logics

module Make (L : Logic.S) :
  Checker_intf.S with module Logic = L = struct
  module Logic = L

  type node =
    | FormulaNode of L.Formula.t * State.t
    | OneStep of
        L.Formula.t L.Formula_spec.t
        * State.t L.Model_spec.t
        * L.one_step_node
  [@@deriving sexp]

  type game =
    ( node
    , Game.Player.t * Game.Priority.t * node list )
    Hashtbl.Poly.t
  [@@deriving sexp]

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
      let node = FormulaNode (formula, state) in
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
        | And (sub_fmla1, sub_fmla2) ->
            add_node Abelard 0
              [
                FormulaNode (sub_fmla1, state)
              ; FormulaNode (sub_fmla2, state)
              ];
            build_formula_node sub_fmla1 state;
            build_formula_node sub_fmla2 state
        | Or (sub_fmla1, sub_fmla2) ->
            add_node Eloise 0
              [
                FormulaNode (sub_fmla1, state)
              ; FormulaNode (sub_fmla2, state)
              ];
            build_formula_node sub_fmla1 state;
            build_formula_node sub_fmla2 state
        | Mu (_x, sub_fmla)
        | Nu (_x, sub_fmla) ->
            add_node Eloise 0
              [ FormulaNode (sub_fmla, state) ];
            build_formula_node sub_fmla state
        | Var x -> (
            match theta x with
            | Some f_def ->
                let priority =
                  alternation_depth x |> Option.value_exn
                in
                add_node Eloise priority
                  [ FormulaNode (f_def, state) ];
                build_formula_node f_def state
            | None ->
                failwith
                  ("Unbound variable: " ^ Var.to_string x))
        | Modal modal_formula ->
            let transition = Hashtbl.find_exn model state in
            let one_step_start_node =
              OneStep (modal_formula, transition, Start)
            in
            add_node Eloise 0 [ one_step_start_node ];
            (* (♥phi, c) -> (♥phi, xi c, Start) *)
            let one_step_game =
              Logic.one_step_game ~transition ~modal_formula
            in
            Hashtbl.iteri one_step_game
              ~f:(fun ~key:_ ~data:(_, _, successors) ->
                List.iter successors ~f:(fun succ ->
                    Hashtbl.set game
                      ~key:
                        (OneStep
                           (modal_formula, transition, succ))
                      ~data:(Eloise, 0, [])));

            Hashtbl.iteri one_step_game
              ~f:(fun
                  ~key:game_node
                  ~data:(owner, priority, successors)
                ->
                (match game_node with
                | Exit _ when not (List.is_empty successors)
                  ->
                    failwith
                      "Exit nodes should not have \
                       successors"
                | _ -> ());
                Hashtbl.set game
                  ~key:
                    (OneStep
                       (modal_formula, transition, game_node))
                  ~data:
                    ( owner
                    , priority
                    , successors
                      (* |> List.filter
                           ~f:(fun (s : L.one_step_node) ->
                             match s with
                             | Start
                             | Exit _ ->
                                 true
                             | Inner _ ->
                                 Hashtbl.mem one_step_game s) *)
                      (* filter away inner nodes not in the one-step game *)
                      |> List.map
                           ~f:(fun (s : L.one_step_node) ->
                             match s with
                             | Start
                             | Inner _ ->
                                 OneStep
                                   ( modal_formula
                                   , transition
                                   , s )
                             | Exit (sub_fmla, s) ->
                                 build_formula_node sub_fmla
                                   s;
                                 FormulaNode (sub_fmla, s))
                    ))
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
            List.map successors
              ~f:(Hashtbl.find_exn key_to_index)
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
    let node_formulas =
      Array.map node_key_array ~f:(fun (node : node) ->
          match node with
          | FormulaNode (formula, _) ->
              L.Formula.to_string formula
          | OneStep (modal_formula, _, _) ->
              L.Formula.to_string (Modal modal_formula))
    in
    let node_states_or_transitions =
      Array.map node_key_array ~f:(fun (node : node) ->
          match node with
          | FormulaNode (_, state) -> State.to_string state
          | OneStep (_, transition, _) ->
              L.Model.transition_to_string transition)
    in
    let node_one_step_info =
      Array.map node_key_array ~f:(fun (node : node) ->
          match node with
          | FormulaNode _ -> ""
          | OneStep (_, _, game_node) ->
              L.sexp_of_one_step_node
                game_node (* TODO: maybe string later *)
              |> Sexp.to_string)
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
    let node_is_one_step =
      Array.map node_key_array ~f:(fun node ->
          match node with
          | OneStep _ -> true
          | FormulaNode _ -> false)
    in
    {
      Checker_intf.num_nodes = n
    ; node_formulas
    ; node_states_or_transitions
    ; node_one_step_info
    ; node_owners
    ; node_priorities
    ; node_successors
    ; winners
    ; strategy = strategy_arr
    ; node_is_one_step
    ; starting_node = starting_index
    ; result
    }

  (* Main model checking entry point — returns full game data *)
  let model_check_full ~verbose ~(model : Logic.Model.t)
      ~(point : State.t) ~(formula : Logic.Formula.t) :
      Checker_intf.game_data =
    let game = build_game ~model ~point ~formula in
    let starting_node = FormulaNode (formula, point) in
    solve_game_internal ~verbose game starting_node

  (* Main model checking entry point — returns bool *)
  let model_check ~verbose ~(model : Logic.Model.t)
      ~(point : State.t) ~(formula : Logic.Formula.t) : bool
      =
    (model_check_full ~verbose ~model ~point ~formula)
      .result
end
