open! Core
open Naive_modcheck_coalg_common

module Relational_ast = Model_intf.Make (struct
  type t = State.t list [@@deriving sexp]
end)

module Graded_ast = Model_intf.Make (struct
  type t = (State.t, int) Hashtbl.Poly.t [@@deriving sexp]
end)

module Probabilistic_ast = struct
  type frac = int * int [@@deriving sexp]

  include Model_intf.Make (struct
    type t = (State.t, frac) Hashtbl.Poly.t [@@deriving sexp]
  end)
end

module Monotone_ast = Model_intf.Make (struct
  type t = State.t list list [@@deriving sexp]
end)
