open! Core

module Make : (A : Model_parser.SPEC)
  (B : Model_parser.SPEC)
  -> Model_parser.SPEC with type 'a t = 'a A.t * 'a B.t
