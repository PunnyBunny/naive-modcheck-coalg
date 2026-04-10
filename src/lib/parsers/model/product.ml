open! Core
open Angstrom
open Naive_modcheck_coalg_parsers_common.Lexer

module Make
    (A : Model_parser.SPEC)
    (B : Model_parser.SPEC) =
struct
  type 'a t = 'a A.t * 'a B.t [@@deriving sexp]

  let to_string (state_a, state_b) ~to_string_parent =
    let state_a_str =
      A.to_string state_a ~to_string_parent
    in
    let state_b_str =
      B.to_string state_b ~to_string_parent
    in
    {%string|(%{state_a_str}, %{state_b_str})|}

  let parser p =
    kw "(" *> lift2 (fun a b -> (a, b)) (A.parser p <* kw ",") (B.parser p) <* kw ")"
end
