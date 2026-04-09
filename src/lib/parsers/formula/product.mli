module Product : functor
  (A : Formula_parser.SPEC)
  (B : Formula_parser.SPEC)
  -> sig
  type 'a t = A of 'a A.t | B of 'a B.t [@@deriving sexp]

  include Formula_parser.SPEC with type 'a t := 'a t
end
