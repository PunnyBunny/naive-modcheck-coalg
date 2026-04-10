open! Core
open Angstrom
open Naive_modcheck_coalg_parsers_common.Lexer

module type EXP_BY_SET_SPEC = sig
  type t [@@deriving sexp, to_string]

  val parser : t Angstrom.t
end

module Make (A : EXP_BY_SET_SPEC) = struct
  type 'a t = A.t * 'a [@@deriving sexp]

  let to_string fmla ~to_string_parent =
    let action, state = fmla in
    let action_str = A.to_string action in
    let state_str = to_string_parent state in
    {%string|[%{action_str}] (%{state_str})|}

  let parser ~formula =
    lift2
      (fun a s -> (a, s))
      (kw "[" *> A.parser <* kw "]")
      formula

  let dual (a, x) = (a, x)
  let map ~f (a, x) = (a, f x)
end
