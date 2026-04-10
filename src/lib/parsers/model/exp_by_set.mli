open! Core

module type EXP_BY_SET_SPEC = sig
  type t [@@deriving sexp, compare, hash, to_string]

  val parser : t Angstrom.t
end

module Make (A : EXP_BY_SET_SPEC) (S : Model_parser.SPEC) :
  Model_parser.SPEC
    with type 'a t = (A.t, 'a S.t) Hashtbl.Poly.t
