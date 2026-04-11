open! Core

module Priority : sig
  type t = int [@@deriving sexp]
end

module Player : sig
  type t = Eloise | Abelard [@@deriving sexp, equal]
end
