open! Core
open Naive_modcheck_coalg_common

include
  Model_parser.SPEC
    with type 'a t = ('a, Frac.t) Hashtbl.Poly.t
