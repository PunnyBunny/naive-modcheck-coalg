open Naive_modcheck_coalg_common
open! Core

module Relational_ast :
  Model_intf.S with type transition = State.t list

module Probabilistic_ast : sig
  type frac = Frac.t [@@deriving sexp]

  include
    Model_intf.S
      with type transition = (State.t, frac) Hashtbl.Poly.t
end
