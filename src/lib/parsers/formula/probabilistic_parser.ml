open! Core
open Product
open Constant
open Distribution
open Exp_by_set
open Composition
open Naive_modcheck_coalg_common
open Naive_modcheck_coalg_parsers_common.Lexer
open Angstrom

module Ap = struct
  type t = Ap.t [@@deriving sexp, to_string]

  let parser = word >>| Ap.of_string
end

module Actions = struct
  type t = Action.t [@@deriving sexp, to_string]

  let parser = word >>| Action.of_string
end

module Formula_spec =
  Product
    (Constant (Ap))
    (Composition (Exp_by_set (Actions)) (Distribution))

module Parser = Formula_parser.Make (Formula_spec)

let parse_formula = Parser.parse_formula
