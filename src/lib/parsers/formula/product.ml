open! Core
open Angstrom
open Naive_modcheck_coalg_parsers_common.Lexer

module Make
    (A : Formula_parser.SPEC)
    (B : Formula_parser.SPEC) =
struct
  module A_closed = Propositional_closure.Make (A)
  module B_closed = Propositional_closure.Make (B)

  type 'a t = A of 'a A_closed.t | B of 'a B_closed.t
  [@@deriving sexp]

  let to_string formula ~to_string_inner =
    match formula with
    | A a ->
        let a_str = A_closed.to_string a ~to_string_inner in
        {%string|[@1] (%{a_str})|}
    | B b ->
        let b_str = B_closed.to_string b ~to_string_inner in
        {%string|[@2] (%{b_str})|}

  let parser ~formula_inner =
    let a =
      kw "[@1]" *> A_closed.parser ~formula_inner >>| fun x -> A x
    in
    let b =
      kw "[@2]" *> B_closed.parser ~formula_inner >>| fun x -> B x
    in
    a <|> b

  let dual = function
    | A a -> A (A_closed.dual a)
    | B b -> B (B_closed.dual b)

  let map ~f = function
    | A a -> A (A_closed.map ~f a)
    | B b -> B (B_closed.map ~f b)
end
