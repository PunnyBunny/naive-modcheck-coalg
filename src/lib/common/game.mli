open! Core

type ('formula, 'state) node =
  | FormulaNode of 'formula * 'state
  | ModalNode of 'formula * 'state list
[@@deriving sexp]

module Priority : sig
  type t = int [@@deriving sexp]
end

module Player : sig
  type t = Eloise | Abelard [@@deriving sexp, equal]
end

(* type ('formula, 'state, 'inner) one_step_node =
  | Start
  | Inner of 'inner
  | Exit of ('formula * 'state)

type ('formula, 'state, 'inner) one_step_game =
  ( ('formula, 'state, 'inner) one_step_node
  , Player.t
    * Priority.t
    * ('formula, 'state, 'inner) one_step_node list )
  Hashtbl.Poly.t *)
