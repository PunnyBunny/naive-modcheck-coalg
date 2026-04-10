module Make : (M : Formula_parser.SPEC)
  -> sig
  type 'a t =
    | True
    | False
    | And of 'a t * 'a t
    | Or of 'a t * 'a t
    | Modal of 'a M.t
  [@@deriving sexp]

  include Formula_parser.SPEC with type 'a t := 'a t
end
