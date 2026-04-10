open! Core

type t [@@deriving string, compare, equal, sexp, hash]

val is_empty : t -> bool
