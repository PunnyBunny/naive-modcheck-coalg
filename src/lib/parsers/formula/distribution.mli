open! Core
open Naive_modcheck_coalg_common

module Make () : sig
  type 'a t =
    | GT of Frac.t * 'a
    | LT of Frac.t * 'a
    | GE of Frac.t * 'a
    | LE of Frac.t * 'a
  [@@deriving sexp]

  include Formula_parser.SPEC with type 'a t := 'a t
end
