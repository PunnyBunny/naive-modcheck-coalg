open Propositional_closure

module Composition : functor
  (A : Formula_parser.SPEC)
  (B : Formula_parser.SPEC)
  ->
  Formula_parser.SPEC
    with type 'a t = 'a Propositional_closure(B).t A.t
