open! Core
open Angstrom
open Naive_modcheck_coalg_parsers_common.Lexer

type 'a t = 'a list [@@deriving sexp]

let to_string states ~to_string_inner =
  let inner =
    String.concat ~sep:", "
      (List.map ~f:to_string_inner states)
  in
  {%string|{%{inner}}|}

let parser ~model_inner =
  kw "{" *> sep_by (kw ",") model_inner <* kw "}"
