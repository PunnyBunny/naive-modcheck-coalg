open Naive_modcheck_coalg_common
open! Core

module Relational_ast : sig
  type t = (State.t, Ap.t list * (Action.t, State.t list) Hashtbl.t) Hashtbl.t
  [@@deriving sexp]
end

module Graded_ast : sig
  type t =
    ( State.t,
      Ap.t list * (Action.t, (State.t, int) Hashtbl.t) Hashtbl.t )
    Hashtbl.t
  [@@deriving sexp]
end

module Probabilistic_ast : sig
  type frac = int * int

  val frac_of_sexp : Sexplib0.Sexp.t -> frac
  val sexp_of_frac : frac -> Sexplib0.Sexp.t

  type t =
    ( State.t,
      Ap.t list * (Action.t, (State.t, frac) Hashtbl.t) Hashtbl.t )
    Hashtbl.t
  [@@deriving sexp]
end

module Monotone_ast : sig
  type t =
    (State.t, Ap.t list * (Action.t, State.t list list) Hashtbl.t) Hashtbl.t
  [@@deriving sexp]
end
