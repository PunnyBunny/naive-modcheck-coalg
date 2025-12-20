open! Core
open Naive_modcheck_coalg_common

(** Common signature for all logics *)
module type S = sig
  type model [@@deriving sexp]
  type modality [@@deriving sexp]
  type model_ast [@@deriving sexp]

  (** in negation normal form *)
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
        (** Returns the Mu/Nu subformula that contains a given variable *)
    alternation_depth : Var.t -> int option;
        (** Returns ad(theta(X)) for a variable X *)
  }

  val model_of_ast : model_ast -> model
  (** Constructs the model out of the abstract syntax tree *)

  val one_step_satisfaction :
    model:model ->
    box_or_diamond:[ `Box | `Diamond ] ->
    state:State.t ->
    states:State.t list ->
    action:Action.t ->
    bool
  (** Returns the solution to the one step satisfaction problem: whether `s \in
      [[♥]]U` (predicate lifting).

      - s is denoted by [state]
      - ♥ is denoted by [box_or_diamond] (TODO: and modality)
      - U is denoted by [states] *)

  val is_atom_in_state : model:model -> state:State.t -> atom:Ap.t -> bool
  val get_states : model:model -> State.t list
  val get_helper_functions : formula -> helper_functions
end

module type LOGIC_SPECIFICATION = sig
  type transition [@@deriving sexp]
  type modality [@@deriving sexp]
  type model_ast [@@deriving sexp]

  type model =
    (State.t, Ap.t list * (Action.t, transition) Hashtbl.Poly.t) Hashtbl.Poly.t
  [@@deriving sexp]

  val model_of_ast : model_ast -> model

  val one_step_satisfaction :
    model:model ->
    box_or_diamond:[ `Box | `Diamond ] ->
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
