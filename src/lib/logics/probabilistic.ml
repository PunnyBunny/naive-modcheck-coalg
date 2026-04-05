open! Core
open Naive_modcheck_coalg_common
module Parsers = Naive_modcheck_coalg_parsers

module Probabilistic_formula = Formula_intf.Make (struct
  open Parsers.Formula.Ast

  type 'a t = 'a probabilistic_modality =
    | GE of Frac.t * 'a
    | LE of Frac.t * 'a
    | GT of Frac.t * 'a
    | LT of Frac.t * 'a
  [@@deriving sexp]

  let to_string m ~to_string_children =
    let sym, frac, child =
      match m with
      | GE (f, c) -> (">=", f, c)
      | LE (f, c) -> ("<=", f, c)
      | GT (f, c) -> (">", f, c)
      | LT (f, c) -> ("<", f, c)
    in
    {%string|[%{sym}%{Frac.to_string frac}]%{to_string_children child}|}

  let map f = function
    | GE (frac, x) -> GE (frac, f x)
    | LE (frac, x) -> LE (frac, f x)
    | GT (frac, x) -> GT (frac, f x)
    | LT (frac, x) -> LT (frac, f x)

  let negate f = function
    | GE (frac, x) -> LT (frac, f x)
    | LT (frac, x) -> GE (frac, f x)
    | GT (frac, x) -> LE (frac, f x)
    | LE (frac, x) -> GT (frac, f x)
end)

module Probabilistic_model = Model_intf.Make (struct
  type t = (State.t, Frac.t) Hashtbl.Poly.t
  [@@deriving sexp]

  let to_string transitions =
    let entries =
      Hashtbl.Poly.fold transitions ~init:[]
        ~f:(fun ~key:state ~data:prob acc ->
          let state = State.to_string state in
          let prob = Frac.to_string prob in
          {%string|%{state}: %{prob}|} :: acc)
    in
    let inner = String.concat ~sep:", " entries in
    {%string|[%{inner}]|}
end)

module M :
  Logic_intf.LOGIC_SPECIFICATION
    with type 'a modality =
      'a Probabilistic_formula.modality
     and type transition = (State.t, Frac.t) Hashtbl.Poly.t
     and module Model = Probabilistic_model
     and module Formula = Probabilistic_formula
     and module Formula_ast = Parsers.Formula.Ast
                              .Probabilistic_ast = struct
  type 'a modality = 'a Probabilistic_formula.modality
  [@@deriving sexp]

  type transition = (State.t, Frac.t) Hashtbl.Poly.t
  [@@deriving sexp]

  module Model = Probabilistic_model
  module Formula = Probabilistic_formula
  module Formula_ast = Parsers.Formula.Ast.Probabilistic_ast

  let get_distribution model state action =
    match Hashtbl.find model state with
    | None -> Hashtbl.Poly.create ()
    | Some (_, transitions) ->
        if Action.is_empty action then (
          (* Merge all action distributions *)
          let merged = Hashtbl.Poly.create () in
          Hashtbl.iter transitions ~f:(fun dist ->
              Hashtbl.iteri dist ~f:(fun ~key ~data ->
                  Hashtbl.update merged key
                    ~f:(fun existing ->
                      match existing with
                      | None -> data
                      | Some prev -> Frac.(prev + data))));
          merged)
        else
          Hashtbl.find transitions action
          |> Option.value ~default:(Hashtbl.Poly.create ())

  let one_step_satisfaction ~(model : Model.t)
      ~(box_or_diamond : [ `Box | `Diamond ])
      ~(state : State.t) ~(states : State.t list)
      ~(action : Action.t) =
    let dist = get_distribution model state action in
    (* Sum the probabilities of successor states that are in [states] *)
    let sum =
      Hashtbl.fold dist ~init:Frac.zero
        ~f:(fun ~key:s ~data:p acc ->
          if List.mem states s ~equal:State.equal then
            Frac.(acc + p)
          else acc)
    in
    match box_or_diamond with
    | `Diamond -> Frac.(sum > Frac.zero)
    | `Box ->
        (* Box: all probability mass goes to [states] *)
        let total =
          Hashtbl.fold dist ~init:Frac.zero
            ~f:(fun ~key:_ ~data:p acc -> Frac.(acc + p))
        in
        Frac.(sum >= total)

  let one_step_game ~model ~state
      ~(modal_formula : Formula.t Formula.modality) =
    let subformula =
      match modal_formula with
      | GE (_, f)
      | LE (_, f)
      | GT (_, f)
      | LT (_, f) ->
          f
    in
    let game = Hashtbl.Poly.create () in
    let start_node =
      Game.FormulaNode (Formula.Modal modal_formula, state)
    in
    let rec powerset lst =
      match lst with
      | [] -> [ [] ]
      | x :: xs ->
          let ps = powerset xs in
          ps @ List.map ps ~f:(fun subset -> x :: subset)
    in
    let all_subsets = powerset (Model.states model) in
    let satisfying_subsets =
      List.filter_map all_subsets ~f:(fun subset ->
          if
            one_step_satisfaction ~model
              ~box_or_diamond:
                (match modal_formula with
                | GE _
                | GT _ ->
                    `Diamond
                | LE _
                | LT _ ->
                    `Box)
              ~state ~states:subset
              ~action:(Action.of_string "")
          then Some subset
          else None)
    in
    Hashtbl.set game ~key:start_node
      ~data:
        ( Game.Player.Eloise
        , 0
        , List.map satisfying_subsets ~f:(fun subset ->
              Game.ModalNode
                (Formula.Modal modal_formula, subset)) );
    List.iter satisfying_subsets ~f:(fun subset ->
        Hashtbl.set game
          ~key:
            (Game.ModalNode
               (Formula.Modal modal_formula, subset))
          ~data:
            ( Game.Player.Abelard
            , 0
            , List.map subset ~f:(fun s ->
                  Game.FormulaNode (subformula, s)) ));
    let exit_nodes =
      List.concat satisfying_subsets
      |> List.dedup_and_sort ~compare:State.compare
      |> List.map ~f:(fun s -> (subformula, s))
    in
    { Game.game; exit_nodes }

  let parse_formula =
    Parsers.Formula.parse_probabilistic_formula

  let parse_model = Parsers.Model.parse_probabilistic_model
end

include Logic.Make (M)
