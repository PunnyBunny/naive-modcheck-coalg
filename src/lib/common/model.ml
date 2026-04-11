open! Core

(** Module type for models parameterized by transition type
*)
module type S = sig
  type 'a transition [@@deriving sexp]
  (** Functor for the coalgebra *)

  type t = (State.t, State.t transition) Hashtbl.Poly.t
  [@@deriving sexp]

  val transition_to_string : State.t transition -> string
  val states : t -> State.t list
  val parse_model : string -> t
end
