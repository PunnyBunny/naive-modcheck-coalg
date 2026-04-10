open! Core
open Angstrom
open Naive_modcheck_coalg_parsers_common.Lexer

module type EXP_BY_SET_SPEC = sig
  type t [@@deriving sexp, compare, hash, to_string]

  val parser : t Angstrom.t
end

module Make (A : EXP_BY_SET_SPEC) (S : Model_parser.SPEC) =
struct
  type 'a t = (A.t, 'a S.t) Hashtbl.Poly.t [@@deriving sexp]

  let to_string tbl ~to_string_inner =
    let entries =
      Hashtbl.Poly.fold tbl ~init:[]
        ~f:(fun ~key:action ~data:state acc ->
          let action_str = A.to_string action in
          let state_str =
            S.to_string ~to_string_inner state
          in
          {%string|%{action_str}: %{state_str}|} :: acc)
    in
    let inner = String.concat ~sep:", " entries in
    {%string|[%{inner}]|}

  let parser ~model_inner =
    let entry =
      lift2
        (fun a s -> (a, s))
        (A.parser <* kw ":")
        (S.parser ~model_inner)
    in
    kw "[" *> sep_by (kw ",") entry
    <* kw "]" >>| Hashtbl.Poly.of_alist_exn
end
