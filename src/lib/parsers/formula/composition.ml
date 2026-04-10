open! Core

module Make
    (A : Formula_parser.SPEC)
    (B : Formula_parser.SPEC) =
struct
  module B_closed = Propositional_closure.Make (B)

  type 'a t = 'a B_closed.t A.t [@@deriving sexp]

  let to_string formula ~to_string_parent =
    let formula_str =
      A.to_string formula
        ~to_string_parent:
          (B_closed.to_string ~to_string_parent)
    in
    {%string|%{formula_str}|}

  let parser ~formula =
    A.parser ~formula:(B_closed.parser ~formula)

  let dual m = A.map ~f:B_closed.dual (A.dual m)
  let map ~f m = A.map ~f:(B_closed.map ~f) m
end
