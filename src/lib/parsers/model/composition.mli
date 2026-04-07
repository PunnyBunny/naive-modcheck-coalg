module Composition : (A : Model_parser.SPEC)
  (B : Model_parser.SPEC)
  -> Model_parser.SPEC with type 'a t = 'a B.t A.t
