open! Core
open Angstrom
open Naive_modcheck_coalg_parsers_common.Lexer

module Make () = struct
  type 'a t = Box of 'a | Diamond of 'a [@@deriving sexp]

  let to_string fmla ~to_string_inner =
    let op_str =
      match fmla with
      | Box _ -> "[]"
      | Diamond _ -> "<>"
    in
    let fmla' =
      match fmla with
      | Box f
      | Diamond f ->
          f
    in
    let fmla'_str = to_string_inner fmla' in
    {%string|%{op_str}(%{fmla'_str})|}

  let parser ~formula_inner =
    let box = lift (fun f -> Box f) (kw "[]" *> formula_inner) in
    let diamond =
      lift (fun f -> Diamond f) (kw "<>" *> formula_inner)
    in
    box <|> diamond

  let dual = function
    | Box x -> Diamond x
    | Diamond x -> Box x

  let map ~f = function
    | Box x -> Box (f x)
    | Diamond x -> Diamond (f x)
end
