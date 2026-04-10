open! Core
open Naive_modcheck_coalg_common

module Make :
  Model_parser.SPEC
    with type 'a t = ('a, Frac.t) Hashtbl.Poly.t
