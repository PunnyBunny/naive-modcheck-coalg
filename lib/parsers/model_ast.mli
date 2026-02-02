open Naive_modcheck_coalg_common
open! Core
module Relational_ast : Model_intf.S with type transition = State.t list

module Graded_ast :
  Model_intf.S with type transition = (State.t, int) Hashtbl.Poly.t

module Probabilistic_ast : sig
  type frac = int * int [@@deriving sexp]

  include Model_intf.S with type transition = (State.t, frac) Hashtbl.Poly.t
end

module Monotone_ast : Model_intf.S with type transition = State.t list list
