open! Core
(** Every token parser eats trailing whitespace. *)

val spacing : unit Angstrom.t
(** Eats whitespace and // comments *)

val tok : 'a Angstrom.t -> 'a Angstrom.t
(** [tok p] runs [p] then eats trailing whitespace *)

val kw : string -> string Angstrom.t

val word : string Angstrom.t
(** Parses an C-like identifier *)

val integer : int Angstrom.t
