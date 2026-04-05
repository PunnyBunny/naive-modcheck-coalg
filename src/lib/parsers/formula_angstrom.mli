open! Core
open Angstrom

module Make (M : Formula_ast.S) : sig
  val make_formula_parser :
    modal:(M.t t -> M.t t) -> string -> M.t
  (** [make_formula_parser ~modal] returns a parser for
      formulas, where [modal] is a function that takes a
      formula parser and returns a parser for modal
      formulas. *)
end
