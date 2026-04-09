open! Core

(** Module type for models parameterized by transition type
*)
module type S = sig
  type 'a transition [@@deriving sexp]
  (** Functor for the coalgebra *)

  type t = (State.t, State.t transition) Hashtbl.Poly.t
  [@@deriving sexp]

  val states : t -> State.t list
  val pretty_print : t -> string
  val parse_model : string -> t
end
