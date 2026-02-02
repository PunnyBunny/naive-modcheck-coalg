open! Core

(** Module type for models parameterized by transition type *)
module type S = sig
  type transition [@@deriving sexp]

  type t =
    (State.t, Ap.t list * (Action.t, transition) Hashtbl.Poly.t) Hashtbl.Poly.t
  [@@deriving sexp]
end

(** Functor to create a model module from a transition type *)
module Make (T : sig
  type t [@@deriving sexp]
end) : S with type transition = T.t = struct
  type transition = T.t [@@deriving sexp]

  type t =
    (State.t, Ap.t list * (Action.t, transition) Hashtbl.Poly.t) Hashtbl.Poly.t
  [@@deriving sexp]
end
