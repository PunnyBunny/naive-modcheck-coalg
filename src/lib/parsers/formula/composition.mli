module Make : functor
  (A : Formula_parser.SPEC)
  (B : Formula_parser.SPEC)
  ->
  Formula_parser.SPEC
    with type 'a t = 'a Propositional_closure.Make(B).t A.t
