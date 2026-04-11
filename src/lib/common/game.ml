open! Core

module Priority = struct
  type t = int [@@deriving sexp]
end

module Player = struct
  type t = Eloise | Abelard [@@deriving sexp, equal]
end
