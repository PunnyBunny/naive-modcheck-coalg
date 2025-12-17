open! Core
open Naive_modcheck_coalg_common

(** Common signature for all logics *)
module type S = sig
  type model [@@deriving sexp]
  type modality [@@deriving sexp]

  type formula =
    | True
    | False
    | Prop of Ap.t
    | Not of formula
    | And of formula * formula
    | Or of formula * formula
    | Diamond of Action.t * modality * formula
    | Box of Action.t * modality * formula
    | Mu of State.t * formula
    | Nu of State.t * formula
  [@@deriving sexp]

  (* val eval_transition : model -> State.t -> Action.t -> transition option
  (** Evaluate model at state to get transition for action *)

  val check : model -> State.t -> formula -> bool
  * Model check a formula at a state *)
end

module type LOGIC_SPECIFICATION = sig
  type transition [@@deriving sexp]
  type modality [@@deriving sexp]
end

module type Intf = sig
  module type S = S
  module type LOGIC_SPECIFICATION = LOGIC_SPECIFICATION

  module Make (_ : LOGIC_SPECIFICATION) : S
end
