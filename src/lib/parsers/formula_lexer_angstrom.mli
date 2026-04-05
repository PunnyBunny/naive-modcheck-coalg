open! Core
(** Every parser eats trailing whitespace. *)

val spacing : unit Angstrom.t
(** Eats whitespace and comments *)

val kw : string -> string Angstrom.t
val word : string Angstrom.t
val integer : int Angstrom.t
