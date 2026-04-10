open! Core
open Angstrom
open Naive_modcheck_coalg_parsers_common.Lexer

module type CONSTANT_SPEC = sig
  type t [@@deriving sexp, to_string]

  val parser : t Angstrom.t
end

module Make (A : CONSTANT_SPEC) = struct
  type 'a t = Normal of A.t | Not of A.t [@@deriving sexp]

  let to_string state ~to_string_inner:_ =
    match state with
    | Normal a -> A.to_string a
    | Not a -> "~" ^ A.to_string a

  (* TODO: review syntax ~ *)
  let parser ~formula_inner:_ =
    let normal = A.parser >>| fun a -> Normal a in
    let not_ = kw "~" *> A.parser >>| fun a -> Not a in
    normal <|> not_

  let dual = function
    | Normal a -> Not a
    | Not a -> Normal a

  let map ~f:_ = function
    | Normal a -> Normal a
    | Not a -> Not a
end
