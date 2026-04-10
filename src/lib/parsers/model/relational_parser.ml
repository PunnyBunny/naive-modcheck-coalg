(* T(X) = P(Ap) * P(X)^A *)
open! Core
open Angstrom
open Naive_modcheck_coalg_common
open Naive_modcheck_coalg_parsers_common.Lexer

module Ap_list = struct
  type t = Ap.t list [@@deriving sexp]

  let to_string aps =
    let ap_strs = List.map aps ~f:Ap.to_string in
    let inner = String.concat ~sep:", " ap_strs in
    {%string|{%{inner}}|}

  let parser =
    kw "{" *> sep_by (kw ",") (word >>| Ap.of_string)
    <* kw "}"
end

module Actions = struct
  type t = Action.t [@@deriving sexp, hash, compare]

  let to_string a =
    if Action.is_empty a then "{}" else Action.to_string a

  let parser =
    kw "{" *> kw "}"
    >>| (fun _ -> Action.of_string "")
    <|> (word >>| Action.of_string)
end

module Model =
  Product.Make
    (Constant.Make
       (Ap_list))
       (Composition.Make
          (Exp_by_set.Make (Actions)) (Powerset))

module Parser = Model_parser.Make (Model)

let parse_model = Parser.parse_model
