open! Core
open Angstrom
open Naive_modcheck_coalg_parsers_common.Lexer
open Naive_modcheck_coalg_common

module Make () = struct
  type 'a t =
    | GT of Frac.t * 'a
    | LT of Frac.t * 'a
    | GE of Frac.t * 'a
    | LE of Frac.t * 'a
  [@@deriving sexp]

  (* e.g. [>=3/4] phi *)

  let to_string fmla ~to_string_inner =
    let op_str =
      match fmla with
      | GT _ -> ">"
      | LT _ -> "<"
      | GE _ -> ">="
      | LE _ -> "<="
    in
    let frac, fmla' =
      match fmla with
      | GT (frac, fmla')
      | LT (frac, fmla')
      | GE (frac, fmla')
      | LE (frac, fmla') ->
          (frac, fmla')
    in
    let fmla'_str = to_string_inner fmla' in
    let frac_str = Frac.to_string frac in
    {%string|%[%{op_str} %{frac_str}] (%{fmla'_str})|}

  let parser ~formula_inner =
    let frac =
      lift2 Frac.make (integer <* kw "/") integer
      <|> lift Frac.of_int integer
    in
    let dispatch (op : [ `GE | `LE | `GT | `LT ]) frac fmla
        =
      match op with
      | `GE -> GE (frac, fmla)
      | `LE -> LE (frac, fmla)
      | `GT -> GT (frac, fmla)
      | `LT -> LT (frac, fmla)
    in
    lift3
      (fun op frac fmla -> op frac fmla)
      (kw "["
      *> (kw ">=" *> return (dispatch `GE)
         <|> kw "<=" *> return (dispatch `LE)
         <|> kw ">" *> return (dispatch `GT)
         <|> kw "<" *> return (dispatch `LT)))
      (frac <* kw "]")
      formula_inner

  let dual = function
    | GT (frac, x) -> LE (frac, x)
    | LE (frac, x) -> GT (frac, x)
    | LT (frac, x) -> GE (frac, x)
    | GE (frac, x) -> LT (frac, x)

  let map ~f = function
    | GT (frac, x) -> GT (frac, f x)
    | LT (frac, x) -> LT (frac, f x)
    | GE (frac, x) -> GE (frac, f x)
    | LE (frac, x) -> LE (frac, f x)
end
