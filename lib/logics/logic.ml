open! Core
open Naive_modcheck_coalg_common
include Logic_intf

module Make (Spec : LOGIC_SPECIFICATION) :
  S with type modality = Spec.modality and type model_ast = Spec.model_ast =
struct
  type modality = Spec.modality [@@deriving sexp]
  type model = Spec.model [@@deriving sexp]
  type model_ast = Spec.model_ast [@@deriving sexp]

  (** TODO: nnf when writing parser *)
  type formula =
    | True
    | False
    | Ap of Ap.t
    | Not of Ap.t
    | Var of Var.t
    | And of formula * formula
    | Or of formula * formula
    | Diamond of Action.t * modality * formula
    | Box of Action.t * modality * formula
    | Mu of Var.t * formula
    | Nu of Var.t * formula
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
  let get_theta formula =
    let theta_table = Hashtbl.Poly.create () in
    let rec build_theta_table = function
      | True | False | Ap _ | Not _ -> ()
      | Var _ -> ()
      | And (f1, f2) | Or (f1, f2) ->
          build_theta_table f1;
          build_theta_table f2
      | Diamond (_, _, f) | Box (_, _, f) -> build_theta_table f
      | Mu (v, f) | Nu (v, f) ->
          Hashtbl.set theta_table ~key:v ~data:f;
          build_theta_table f
    in
    build_theta_table formula;
    fun v -> Hashtbl.find theta_table v
end
