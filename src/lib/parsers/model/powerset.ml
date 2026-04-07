open! Core
open Angstrom
open Naive_modcheck_coalg_parsers_common.Lexer

module Powerset = struct
  type 'a t = 'a list [@@deriving sexp]

  let to_string states ~to_string_parent =
    let inner =
      String.concat ~sep:", "
        (List.map ~f:to_string_parent states)
    in
    {%string|{%{inner}}|}
  
  let parser p = kw "{" *> sep_by (kw ",") p <* kw "}"
end

