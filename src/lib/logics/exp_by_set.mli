open! Core
open Naive_modcheck_coalg_parsers_model
open Naive_modcheck_coalg_parsers_formula

module type EXP_BY_SET_SPEC = sig
  type t [@@deriving sexp, compare, hash, to_string]

  val parser : t Angstrom.t
end

module Make (A : EXP_BY_SET_SPEC) :
  Logic_intf.LOGIC_SPECIFICATION
    with module Model_spec = Model_parsers.Exp_by_set.Make(A)
     and module Formula_spec = Formula_parsers.Exp_by_set.Make(A)
