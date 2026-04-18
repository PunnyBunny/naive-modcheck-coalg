open! Core

module type CONSTANT_SPEC = sig
  type t [@@deriving sexp, to_string]

  val parser : t Angstrom.t
  val default : unit -> t
end

module Make (A : CONSTANT_SPEC) :
  Model_parser.SPEC with type 'a t = A.t
