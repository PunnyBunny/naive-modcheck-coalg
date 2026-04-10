open! Core
open Angstrom
open Naive_modcheck_coalg_parsers_common.Lexer

module type EXP_BY_SET_SPEC = sig
  type t [@@deriving sexp, to_string]

  val parser : t Angstrom.t
end

module Make (A : EXP_BY_SET_SPEC) (S : Formula_parser.SPEC) =
struct
  module S_closed = Propositional_closure.Make (S)

  type 'a t = A.t * 'a S_closed.t [@@deriving sexp]

  let to_string fmla ~to_string_inner =
    let action, state = fmla in
    let action_str = A.to_string action in
    let state_str =
      S_closed.to_string state ~to_string_inner
    in
    {%string|[%{action_str}] (%{state_str})|}

  let parser ~formula_inner =
    lift2
      (fun a s -> (a, s))
      (kw "[" *> A.parser <* kw "]")
      (S_closed.parser ~formula_inner)

  let dual (a, x) = (a, x)
  let map ~f (a, x) = (a, S_closed.map ~f x)
end
