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

  type helper_functions = {
    theta : Var.t -> formula option;
    alternation_depth : Var.t -> int option;
  }

  let model_of_ast = Spec.model_of_ast
  let one_step_satisfaction = Spec.one_step_satisfaction

  let is_atom_in_state ~(model : model) ~(state : State.t) ~(atom : Ap.t) : bool
      =
    match Hashtbl.find model state with
    | None -> false
    | Some (atoms, _) -> List.mem atoms atom ~equal:Ap.equal

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

  let get_alternation_depth formula =
    let alternation_depth_table = Hashtbl.Poly.create () in
    let rec build_table = function
      | True | False | Ap _ | Not _ | Var _ -> 0
      | And (f1, f2) | Or (f1, f2) -> Int.max (build_table f1) (build_table f2)
      | Diamond (_, _, f) | Box (_, _, f) -> build_table f
      | Mu (v, f) ->
          let depth = build_table f in
          let new_depth = if depth mod 2 = 0 then depth + 1 else depth in
          Hashtbl.set alternation_depth_table ~key:v ~data:new_depth;
          new_depth
      | Nu (v, f) ->
          let depth = build_table f in
          let new_depth = if depth mod 2 = 1 then depth + 1 else depth in
          Hashtbl.set alternation_depth_table ~key:v ~data:new_depth;
          new_depth
    in
    ignore (build_table formula);
    fun f -> Hashtbl.find alternation_depth_table f

  let get_helper_functions formula =
    {
      theta = get_theta formula;
      alternation_depth = get_alternation_depth formula;
    }
end
