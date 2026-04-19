open! Core
open Angstrom
open Naive_modcheck_coalg_parsers_common.Lexer
open Naive_modcheck_coalg_common

type 'a t = GT of Frac.t * 'a | GE of Frac.t * 'a
[@@deriving sexp]

(* e.g. [>=3/4] phi *)

let to_string fmla ~to_string_inner =
  let op_str =
    match fmla with
    | GT _ -> ">"
    | GE _ -> ">="
  in
  let frac, fmla' =
    match fmla with
    | GT (frac, fmla')
    | GE (frac, fmla') ->
        (frac, fmla')
  in
  let fmla'_str = to_string_inner fmla' in
  let frac_str = Frac.to_string frac in
  {%string|[%{op_str} %{frac_str}] (%{fmla'_str})|}

let parser ~formula_inner =
  let frac =
    lift2 Frac.make (integer <* kw "/") integer
    <|> lift Frac.of_int integer
  in
  let dispatch (op : [ `GE | `GT ]) frac fmla =
    match op with
    | `GE -> GE (frac, fmla)
    | `GT -> GT (frac, fmla)
  in
  lift3
    (fun op frac fmla -> op frac fmla)
    (kw "["
    *> (kw ">=" *> return (dispatch `GE)
       <|> kw ">" *> return (dispatch `GT)))
    (frac <* kw "]")
    formula_inner

let dual =
  let open Frac in
  function
  | GT (frac, x) -> GE (of_int 1 - frac, x)
  | GE (frac, x) -> GT (of_int 1 - frac, x)

let map ~f = function
  | GT (frac, x) -> GT (frac, f x)
  | GE (frac, x) -> GE (frac, f x)
