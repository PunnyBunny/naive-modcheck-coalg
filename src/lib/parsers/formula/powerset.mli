open! Core

module Powerset : sig
  type 'a t = Box of 'a | Diamond of 'a [@@deriving sexp]

  include Formula_parser.SPEC with type 'a t := 'a t
end
