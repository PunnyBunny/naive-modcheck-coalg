open! Core

module type EXP_BY_SET_SPEC = sig
  type t [@@deriving sexp, compare, hash, to_string]

  val parser : t Angstrom.t
end

module Make : EXP_BY_SET_SPEC
  -> Logic_intf.LOGIC_SPECIFICATION
  -> Logic_intf.LOGIC_SPECIFICATION
