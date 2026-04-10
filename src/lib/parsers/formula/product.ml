open! Core
open Angstrom
open Naive_modcheck_coalg_parsers_common.Lexer

module Make
    (A : Formula_parser.SPEC)
    (B : Formula_parser.SPEC) =
struct
  type 'a t = A of 'a A.t | B of 'a B.t [@@deriving sexp]

  let to_string formula ~to_string_inner =
    match formula with
    | A a ->
        let a_str = A.to_string a ~to_string_inner in
        {%string|[@1] (%{a_str})|}
    | B b ->
        let b_str = B.to_string b ~to_string_inner in
        {%string|[@2] (%{b_str})|}

  let parser ~formula_inner =
    let a =
      kw "[@1]" *> A.parser ~formula_inner >>| fun x -> A x
    in
    let b =
      kw "[@2]" *> B.parser ~formula_inner >>| fun x -> B x
    in
    a <|> b

  let dual = function
    | A a -> A (A.dual a)
    | B b -> B (B.dual b)

  let map ~f = function
    | A a -> A (A.map ~f a)
    | B b -> B (B.map ~f b)
end
