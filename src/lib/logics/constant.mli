open! Core
open Naive_modcheck_coalg_parsers_model
open Naive_modcheck_coalg_parsers_formula

module type CONSTANT_SPEC = sig
  type t [@@deriving sexp, to_string]

  val parser : t Angstrom.t
end

module Make (A : CONSTANT_SPEC) :
  Logic_intf.LOGIC_SPECIFICATION
    with module Model_spec = Model_parsers.Constant.Make(A)
     and module Formula_spec = Formula_parsers.Constant.Make(A)
