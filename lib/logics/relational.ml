open! Core
open Naive_modcheck_coalg_common
open Naive_modcheck_coalg_parsers.Model

module M :
  Logic_intf.LOGIC_SPECIFICATION
    with type modality = unit
     and type model_ast = Relational_ast.t = struct
  type transition = State.t list [@@deriving sexp]
  type modality = unit [@@deriving sexp]
  type model_ast = Relational_ast.t [@@deriving sexp]

  type model =
    (State.t, Ap.t list * (Action.t, transition) Hashtbl.Poly.t) Hashtbl.Poly.t
  [@@deriving sexp]

  let next_states model state action =
    match Hashtbl.find model state with
    | None -> []
    | Some (_, transitions) ->
        if Action.is_empty action then Hashtbl.data transitions |> List.concat
        else
          Hashtbl.find transitions action
          |> Option.value_exn ~message:"No transition found"

  let model_of_ast (model_ast : model_ast) : model = model_ast

  let one_step_satisfaction ~(model : model)
      ~(box_or_diamond : [ `Box | `Diamond ]) ~(state : State.t)
      ~(states : State.t list) ~(action : Action.t) =
    let successors = next_states model state action in
    match box_or_diamond with
    | `Diamond ->
        List.exists successors ~f:(fun s ->
            List.mem states s ~equal:State.equal)
    | `Box ->
        List.for_all successors ~f:(fun s ->
            List.mem states s ~equal:State.equal)
end

include Logic.Make (M)
