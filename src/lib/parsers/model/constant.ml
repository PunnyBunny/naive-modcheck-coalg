open! Core

module type CONSTANT_SPEC = sig
  type t [@@deriving sexp, to_string]

  val parser : t Angstrom.t
end

module Constant (A : CONSTANT_SPEC) = struct
  type 'a t = A.t [@@deriving sexp]

  let to_string state ~to_string_parent:_ =
    A.to_string state

  let parser _ = A.parser
end
