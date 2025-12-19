open! Core
open Naive_modcheck_coalg_parsers.Model

(** Relational modal logic *)

include Logic.S with type modality = unit and type model_ast = Relational_ast.t
