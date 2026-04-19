open! Core

module type CONSTANT_SPEC = sig
  type t [@@deriving sexp, to_string, equal]

  val parser : t Angstrom.t
  val default : unit -> t
end

module Make_list (A : sig
  type t [@@deriving sexp, to_string, equal]

  val parser : t Angstrom.t
end) : CONSTANT_SPEC with type t = A.t list

module Make (A : CONSTANT_SPEC) :
  Model_parser.SPEC with type 'a t = A.t
