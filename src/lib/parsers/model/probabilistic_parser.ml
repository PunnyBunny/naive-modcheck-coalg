(* T(X) = P(Ap) * D(X)^A *)
open! Core
open Product
open Constant
open Distribution
open Exp_by_set
open Composition
open Angstrom
open Naive_modcheck_coalg_common
open Naive_modcheck_coalg_parsers_common.Lexer

module Ap_list = struct
  type t = Ap.t list [@@deriving sexp]

  let to_string aps =
    let ap_strs = List.map aps ~f:Ap.to_string in
    String.concat ~sep:", " ap_strs

  let parser =
    kw "{"
    *> sep_by (Angstrom.char ',') (word >>| Ap.of_string)
    <* kw "}"
end

module Actions = struct
  type t = Action.t
  [@@deriving sexp, to_string, hash, compare]

  let parser = word >>| Action.of_string
end

module Model =
  Product
    (Constant
       (Ap_list))
       (Composition (Exp_by_set (Actions)) (Distribution))

module Parser = Model_parser.Make (Model)

let parse_model = Parser.parse_model
