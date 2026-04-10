open! Core

module type CONSTANT_SPEC = sig
  type t [@@deriving sexp, to_string]

  val parser : t Angstrom.t
end

module Make (A : CONSTANT_SPEC) : sig
  type 'a t = Normal of A.t | Not of A.t [@@deriving sexp]

  include Formula_parser.SPEC with type 'a t := 'a t
end
