module Make : functor
  (A : Formula_parser.SPEC)
  (B : Formula_parser.SPEC)
  -> sig
  type 'a t =
    | A of 'a Propositional_closure.Make(A).t
    | B of 'a Propositional_closure.Make(B).t
  [@@deriving sexp]

  include Formula_parser.SPEC with type 'a t := 'a t
end
