open! Core

type t [@@deriving string, compare, sexp, hash]

val is_empty : t -> bool
