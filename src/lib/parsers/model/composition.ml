open! Core

module Composition
    (A : Model_parser.SPEC)
    (B : Model_parser.SPEC) =
struct
  type 'a t = 'a B.t A.t [@@deriving sexp]

  let to_string state ~to_string_parent =
    let inner =
      A.to_string state
        ~to_string_parent:(B.to_string ~to_string_parent)
    in
    {%string|%{inner}|}

  let parser p = A.parser (B.parser p)
end
