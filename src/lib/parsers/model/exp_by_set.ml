open! Core
open Angstrom
open Naive_modcheck_coalg_parsers_common.Lexer

module type EXP_BY_SET_SPEC = sig
  type t [@@deriving sexp, compare, hash, to_string]

  val parser : t Angstrom.t
end

module Exp_by_set (A : EXP_BY_SET_SPEC) = struct
  type 'a t = (A.t, 'a) Hashtbl.Poly.t [@@deriving sexp]

  let to_string tbl ~to_string_parent =
    let entries =
      Hashtbl.Poly.fold tbl ~init:[]
        ~f:(fun ~key:action ~data:state acc ->
          let action_str = A.to_string action in
          let state_str = to_string_parent state in
          {%string|%{action_str}: %{state_str}|} :: acc)
    in
    let inner = String.concat ~sep:", " entries in
    {%string|[%{inner}]|}

  let parser p =
    let entry =
      lift2 (fun a s -> (a, s)) (A.parser <* kw ":") p
    in
    kw "[" *> sep_by (kw ",") entry
    <* kw "]" >>| Hashtbl.Poly.of_alist_exn
end
