open! Core
open Naive_modcheck_coalg_common

module Ap : sig
  type t = Ap.t [@@deriving sexp, equal]

  val to_string : t -> string
  val parser : t Angstrom.t
end

module Actions : sig
  type t = Action.t [@@deriving sexp, hash, equal, compare]

  val to_string : t -> string
  val parser : t Angstrom.t
end
