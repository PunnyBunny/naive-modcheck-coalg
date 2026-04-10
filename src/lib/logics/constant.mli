open! Core

module type CONSTANT_SPEC = sig
  type t [@@deriving sexp, to_string, equal]

  val parser : t Angstrom.t
end

module Make : CONSTANT_SPEC ->
  Logic_intf.LOGIC_SPECIFICATION
