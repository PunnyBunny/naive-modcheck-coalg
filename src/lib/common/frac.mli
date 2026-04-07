open! Core

type t [@@deriving sexp]

val make : int -> int -> t
val of_int : int -> t
val zero : t
val compare : t -> t -> int
val ( + ) : t -> t -> t
val ( >= ) : t -> t -> bool
val ( > ) : t -> t -> bool
val to_string : t -> string
