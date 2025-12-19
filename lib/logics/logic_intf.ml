open! Core
open Naive_modcheck_coalg_common

(** Common signature for all logics *)
module type S = sig
  type model [@@deriving sexp]
  type modality [@@deriving sexp]
  type model_ast [@@deriving sexp]

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

  val model_of_ast : model_ast -> model

  val predicate_lifting :
    box_or_diamond:[ `Box | `Diamond ] ->
    model:model ->
    state:State.t ->
    states:State.t list ->
    action:Action.t ->
    bool

  val is_atom_in_state : model:model -> state:State.t -> atom:Ap.t -> bool
  val get_states : model:model -> State.t list

  val theta : Var.t -> formula option
  (** Returns the Mu/Nu subformula that contains a given variable *)

  (* val eval_transition : model -> State.t -> Action.t -> transition option
  (** Evaluate model at state to get transition for action *)

  val check : model -> State.t -> formula -> bool
  * Model check a formula at a state *)
end

module type LOGIC_SPECIFICATION = sig
  type transition [@@deriving sexp]
  type modality [@@deriving sexp]
  type model_ast [@@deriving sexp]

  type model =
    (State.t, Ap.t list * (Action.t, transition) Hashtbl.Poly.t) Hashtbl.Poly.t
  [@@deriving sexp]

  val model_of_ast : model_ast -> model

  val predicate_lifting :
    box_or_diamond:[ `Box | `Diamond ] ->
    model:model ->
    state:State.t ->
    states:State.t list ->
    action:Action.t ->
    bool
end

module type Intf = sig
  module type S = S
  module type LOGIC_SPECIFICATION = LOGIC_SPECIFICATION

  module Make (Spec : LOGIC_SPECIFICATION) :
    S with type modality = Spec.modality and type model_ast = Spec.model_ast
end
