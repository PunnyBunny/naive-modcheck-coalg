open! Core

type ('formula, 'state) node =
  | FormulaNode of 'formula * 'state
  | ModalNode of 'formula * 'state list
[@@deriving sexp]

module Priority = struct
  type t = int [@@deriving sexp]
end

module Player = struct
  type t = Eloise | Abelard [@@deriving sexp]
end

type ('formula, 'state) one_step_game = {
    game :
      ( ('formula, 'state) node
      , Player.t * Priority.t * ('formula, 'state) node list
      )
      Hashtbl.Poly.t
  ; exit_nodes : ('formula * 'state) list
}
