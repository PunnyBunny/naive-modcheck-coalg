open! Core

module Make (A : Model_parser.SPEC) (B : Model_parser.SPEC) =
struct
  type 'a t = 'a B.t A.t [@@deriving sexp]

  let to_string state ~to_string_inner =
    let inner =
      A.to_string state
        ~to_string_inner:(B.to_string ~to_string_inner)
    in
    {%string|%{inner}|}

  let parser ~model_inner =
    A.parser ~model_inner:(B.parser ~model_inner)
end
