open! Core

module type EXP_BY_SET_SPEC = sig
  type t [@@deriving sexp, to_string]

  val parser : t Angstrom.t
end

module Exp_by_set (A : EXP_BY_SET_SPEC) :
  Formula_parser.SPEC with type 'a t = A.t * 'a
