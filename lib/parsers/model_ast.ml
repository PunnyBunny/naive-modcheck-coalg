open! Core
open Naive_modcheck_coalg_common

module Relational_ast = struct
  type t =
    ( State.t,
      Ap.t list * (Action.t, State.t list) Hashtbl.Poly.t )
    Hashtbl.Poly.t
  [@@deriving sexp]
end

module Graded_ast = struct
  type t =
    ( State.t,
      Ap.t list * (Action.t, (State.t, int) Hashtbl.Poly.t) Hashtbl.Poly.t )
    Hashtbl.Poly.t
  [@@deriving sexp]
end

module Probabilistic_ast = struct
  type frac = int * int [@@deriving sexp]

  type t =
    ( State.t,
      Ap.t list * (Action.t, (State.t, frac) Hashtbl.Poly.t) Hashtbl.Poly.t )
    Hashtbl.Poly.t
  [@@deriving sexp]
end

module Monotone_ast = struct
  type t =
    ( State.t,
      Ap.t list * (Action.t, State.t list list) Hashtbl.Poly.t )
    Hashtbl.Poly.t
  [@@deriving sexp]
end
