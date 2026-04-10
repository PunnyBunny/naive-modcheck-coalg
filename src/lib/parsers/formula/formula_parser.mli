open! Core
open Naive_modcheck_coalg_common

module type SPEC = sig
  type 'a t [@@deriving sexp]

  val to_string :
    'a t -> to_string_inner:('a -> string) -> string

  val parser :
    formula_inner:'a Angstrom.t -> 'a t Angstrom.t

  val dual : 'a t -> 'a t
  val map : f:('a -> 'b) -> 'a t -> 'b t
end

module Make (M : SPEC) :
  Formula.S with type 'a modality = 'a M.t
