(** Focus on relational, graded, probabilistic, monotone mu-calculus for now

    - relational: STATE -> (AP list * (ACTION -> STATE list))
    - graded: STATE -> (AP list * (ACTION -> (STATE -> int)))
    - probabilistic: STATE -> (AP list * (ACTION -> (STATE -> frac)))
    - monotone: STATE -> (AP list * (ACTION -> STATE list list)) *)

open! Core

module Action : sig
  type t [@@deriving string, compare, sexp, hash]
end

module Ap : sig
  type t [@@deriving string, compare, sexp, hash]
end

module State : sig
  type t [@@deriving string, compare, sexp, hash]
end

module Relational_ast : sig
  type t =
    ( State.t,
      Ap.t list * (Action.t, State.t list) Hashtbl.Poly.t )
    Hashtbl.Poly.t
  [@@deriving sexp]
end

module Graded_ast : sig
  type t =
    ( State.t,
      Ap.t list * (Action.t, (State.t, int) Hashtbl.Poly.t) Hashtbl.Poly.t )
    Hashtbl.Poly.t
  [@@deriving sexp]
end

module Probabilistic_ast : sig
  type frac = int * int [@@deriving sexp]

  type t =
    ( State.t,
      Ap.t list * (Action.t, (State.t, frac) Hashtbl.Poly.t) Hashtbl.Poly.t )
    Hashtbl.Poly.t
  [@@deriving sexp]
end

module Monotone_ast : sig
  type t =
    ( State.t,
      Ap.t list * (Action.t, State.t list list) Hashtbl.Poly.t )
    Hashtbl.Poly.t
  [@@deriving sexp]
end
