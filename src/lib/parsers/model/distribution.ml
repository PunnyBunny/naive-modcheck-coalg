open! Core
open Angstrom
open Naive_modcheck_coalg_parsers_common.Lexer
open Naive_modcheck_coalg_common

module Make = struct
  type 'a t = ('a, Frac.t) Hashtbl.Poly.t [@@deriving sexp]

  let to_string tbl ~to_string_parent =
    let entries =
      Hashtbl.Poly.fold tbl ~init:[]
        ~f:(fun ~key:state ~data:frac acc ->
          let state_str = to_string_parent state in
          let frac_str = Frac.to_string frac in
          {%string|%{state_str}: %{frac_str}|} :: acc)
    in
    let inner = String.concat ~sep:", " entries in
    {%string|[%{inner}]|}

  let parser p =
    let frac =
      lift2 Frac.make (integer <* kw "/") integer
      <|> lift Frac.of_int integer
    in
    let entry =
      lift2 (fun s f -> (s, f)) (p <* kw ":") frac
    in
    kw "[" *> sep_by (kw ",") entry
    <* kw "]" >>| Hashtbl.Poly.of_alist_exn
end
