open! Core

module type CONSTANT_SPEC = sig
  type t [@@deriving sexp, to_string]

  val parser : t Angstrom.t
  val default : unit -> t
end

module Make (A : CONSTANT_SPEC) = struct
  type 'a t = A.t [@@deriving sexp]

  let to_string state ~to_string_inner:_ = A.to_string state
  let parser ~model_inner:_ = A.parser
  let default () = A.default ()
end
