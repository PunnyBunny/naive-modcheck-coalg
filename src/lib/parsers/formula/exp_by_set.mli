open! Core

module type EXP_BY_SET_SPEC = sig
  type t [@@deriving sexp, to_string]

  val parser : t Angstrom.t
end

module Make
    (A : EXP_BY_SET_SPEC)
    (S : Formula_parser.SPEC) :
  Formula_parser.SPEC
    with type 'a t =
      A.t * 'a Propositional_closure.Make(S).t
