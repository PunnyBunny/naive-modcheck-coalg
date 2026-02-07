open! Core
open Naive_modcheck_coalg_common
module Parsers = Naive_modcheck_coalg_parsers

module Probabilistic_formula = Formula_intf.Make (struct
  type t = Frac.t [@@deriving sexp]
end)

module Probabilistic_model = Model_intf.Make (struct
  type t = (State.t, Frac.t) Hashtbl.Poly.t [@@deriving sexp]
end)

module M :
  Logic_intf.LOGIC_SPECIFICATION
    with type modality = Frac.t
     and type transition = (State.t, Frac.t) Hashtbl.Poly.t
     and module Model = Probabilistic_model
     and module Formula = Probabilistic_formula
     and module Formula_ast = Parsers.Formula.Ast.Probabilistic_ast = struct
  type modality = Frac.t [@@deriving sexp]
  type transition = (State.t, Frac.t) Hashtbl.Poly.t [@@deriving sexp]

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
                  Hashtbl.update merged key ~f:(fun existing ->
                      match existing with
                      | None -> data
                      | Some prev -> Frac.(prev + data))));
          merged)
        else
          Hashtbl.find transitions action
          |> Option.value ~default:(Hashtbl.Poly.create ())

  let one_step_satisfaction ~(model : Model.t)
      ~(box_or_diamond : [ `Box | `Diamond ]) ~(state : State.t)
      ~(states : State.t list) ~(action : Action.t) =
    let dist = get_distribution model state action in
    (* Sum the probabilities of successor states that are in [states] *)
    let sum =
      Hashtbl.fold dist ~init:Frac.zero ~f:(fun ~key:s ~data:p acc ->
          if List.mem states s ~equal:State.equal then Frac.(acc + p) else acc)
    in
    match box_or_diamond with
    | `Diamond -> Frac.(sum > Frac.zero)
    | `Box ->
        (* Box: all probability mass goes to [states] *)
        let total =
          Hashtbl.fold dist ~init:Frac.zero ~f:(fun ~key:_ ~data:p acc ->
              Frac.(acc + p))
        in
        Frac.(sum >= total)

  let parse_formula = Parsers.Formula.parse_probabilistic_formula
  let parse_model = Parsers.Model.parse_probabilistic_model
end

include Logic.Make (M)
