(* T(X) = P(Ap) * P(X)^A *)
open! Core
open Naive_modcheck_coalg_common

module Ap_list :
  Constant.CONSTANT_SPEC with type t = Ap.t list

module Actions :
  Exp_by_set.EXP_BY_SET_SPEC with type t = Action.t

module Model :
  Model_parser.SPEC
    with type 'a t =
      Ap_list.t
      * ( Action.t
        , ('a, Frac.t) Hashtbl.Poly.t )
        Hashtbl.Poly.t

val parse_model :
  string -> (State.t, State.t Model.t) Hashtbl.Poly.t
