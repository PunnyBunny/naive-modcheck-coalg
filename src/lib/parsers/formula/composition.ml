open! Core

module Make
    (A : Formula_parser.SPEC)
    (B : Formula_parser.SPEC) =
struct
  module B_closed = Propositional_closure.Make (B)

  type 'a t = 'a B_closed.t A.t [@@deriving sexp]

  let to_string formula ~to_string_inner =
    let formula_str =
      A.to_string formula
        ~to_string_inner:
          (B_closed.to_string ~to_string_inner)
    in
    {%string|%{formula_str}|}

  let parser ~formula_inner =
    A.parser ~formula_inner:(B_closed.parser ~formula_inner)

  let dual m = A.map ~f:B_closed.dual (A.dual m)
  let map ~f m = A.map ~f:(B_closed.map ~f) m
end
