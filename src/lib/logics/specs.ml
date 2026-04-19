open! Core
open Naive_modcheck_coalg_common
open Naive_modcheck_coalg_parsers_common.Lexer
open Angstrom

module Ap = struct
  type t = Ap.t [@@deriving sexp, equal]

  let to_string = Ap.to_string
  let parser = word >>| Ap.of_string
end

module Actions = struct
  type t = Action.t [@@deriving sexp, hash, equal, compare]

  let to_string a =
    if Action.is_empty a then "{}" else Action.to_string a

  let parser = word >>| Action.of_string
end
