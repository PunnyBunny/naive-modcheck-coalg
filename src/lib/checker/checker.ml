open! Core
open Naive_modcheck_coalg_common
open Naive_modcheck_coalg_logics

module Make (L : Logic.S) :
  Checker_intf.S with module Logic = L = struct
  module Logic = L

  (* Game node types *)
  type node =
    | FormulaNode of Logic.Formula.t * State.t
    | ModalNode of Logic.Formula.t * State.t list
  [@@deriving sexp]

  module Priority = struct
    type t = int [@@deriving sexp]
  end

  module Player = struct
    type t = Eloise | Abelard [@@deriving sexp]
  end

  type game =
    (node, Player.t * Priority.t * node list) Hashtbl.Poly.t
  [@@deriving sexp]

  (* Helper: powerset *)
  let rec powerset lst =
    match lst with
    | [] -> [ [] ]
    | x :: xs ->
        let ps = powerset xs in
        ps @ List.map ps ~f:(fun subset -> x :: subset)

  let pretty_print_formula node =
    match node with
    | FormulaNode (formula, _)
    | ModalNode (formula, _) ->
        Logic.Formula.pretty_print formula

  (* Build parity game from model and formula *)
  let build_game ~(model : Logic.Model.t) ~(point : State.t)
      ~(formula : Logic.Formula.t) : game =
    let game = Hashtbl.Poly.create () in
    let states = Logic.get_states ~model in
    let powerset_of_states = powerset states in
    let { theta; alternation_depth } :
        Logic.helper_functions =
      Logic.get_helper_functions formula
    in

    let rec build_formula_node (formula : Logic.Formula.t)
        (state : State.t) : unit =
      let node = FormulaNode (formula, state) in
      if Hashtbl.mem game node then ()
      else
        let add_node (owner : Player.t) priority successors
            =
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
        | Mu (x, sub_fmla)
        | Nu (x, sub_fmla) ->
            let priority =
              alternation_depth x |> Option.value_exn
            in
            add_node Eloise priority
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
        | Diamond (a, _m, sub_fmla) ->
            let satisfying_sets =
              List.filter powerset_of_states
                ~f:(fun state_set ->
                  Logic.one_step_satisfaction
                    ~box_or_diamond:`Diamond ~model ~state
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
              List.filter powerset_of_states
                ~f:(fun state_set ->
                  Logic.one_step_satisfaction
                    ~box_or_diamond:`Box ~model ~state
                    ~states:state_set ~action:a)
            in
            let modal_nodes =
              List.map satisfying_sets ~f:(fun states ->
                  ModalNode (sub_fmla, states))
            in
            add_node Eloise 0 modal_nodes;
            (* This was wrong, can be put into dissertation *)
            List.iter satisfying_sets ~f:(fun states ->
                build_modal_node sub_fmla states)
    (*  (* Use only the minimal Box-satisfying set (intersection of all satisfying
               sets = exact successors for relational, distribution support for
               probabilistic). This prevents Abelard from challenging with non-successor
               states drawn from artificially large witness sets. *)
            (match satisfying_sets with
            | [] -> add_node Abelard 0 []
            | first :: rest ->
                let min_set =
                  List.fold rest ~init:first ~f:(fun acc s_set ->
                      List.filter acc ~f:(fun s ->
                          List.mem s_set s ~equal:State.equal))
                in
                add_node Abelard 0 [ ModalNode (sub_fmla, min_set) ];
                build_modal_node sub_fmla min_set)*)
    and build_modal_node (sub_fmla : Logic.Formula.t)
        (states : State.t list) : unit =
      let node = ModalNode (sub_fmla, states) in
      if Hashtbl.mem game node then ()
      else begin
        let formula_nodes =
          List.map states ~f:(fun state ->
              FormulaNode (sub_fmla, state))
        in
        Hashtbl.set game ~key:node
          ~data:(Abelard, 0, formula_nodes);
        List.iter states ~f:(fun state ->
            build_formula_node sub_fmla state)
      end
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
            | Eloise -> Paritygame.plr_Even
            | Abelard -> Paritygame.plr_Odd
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
          | Eloise -> "Eloise"
          | Abelard -> "Abelard")
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
          | ModalNode _ -> true
          | _ -> false)
    in
    let node_states =
      Array.map node_key_array ~f:(fun node ->
          match node with
          | FormulaNode (_, state) ->
              [ State.to_string state ]
          | ModalNode (_, states) ->
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
    let starting_node = FormulaNode (formula, point) in
    solve_game_internal ~verbose game starting_node

  (* Main model checking entry point — returns bool *)
  let model_check ~verbose ~(model : Logic.Model.t)
      ~(point : State.t) ~(formula : Logic.Formula.t) : bool
      =
    (model_check_full ~verbose ~model ~point ~formula)
      .result
end
