open! Core
open Naive_modcheck_coalg_common
include Logic_intf

module Make (Spec : LOGIC_SPECIFICATION) :
  S with type modality = Spec.modality and type model_ast = Spec.model_ast =
struct
  type modality = Spec.modality [@@deriving sexp]
  type model = Spec.model [@@deriving sexp]
  type model_ast = Spec.model_ast [@@deriving sexp]

  type formula =
    | True
    | False
    | Ap of Ap.t
    | Not of formula
    | And of formula * formula
    | Or of formula * formula
    | Diamond of Action.t * modality * formula
    | Box of Action.t * modality * formula
    | Mu of Var.t * formula
    | Nu of Var.t * formula
    | Var of Var.t
  [@@deriving sexp]

  let model_of_ast = Spec.model_of_ast
  let predicate_lifting = Spec.predicate_lifting

  let is_atom_in_state ~(model : model) ~(state : State.t) ~(atom : Ap.t) : bool
      =
    match Hashtbl.find model state with
    | None -> false
    | Some (props, _) -> List.mem props atom ~equal:phys_equal

  let get_states ~(model : model) : State.t list =
    Hashtbl.to_alist model |> List.map ~f:fst

  (* TODO: implement *)
  let theta (_ : Var.t) : formula option = None
end
