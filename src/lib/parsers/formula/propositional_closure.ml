open! Core
open Angstrom
open Naive_modcheck_coalg_parsers_common.Lexer

let chain_binary ~op e =
  e >>= fun init ->
  many
    ( op >>= fun f ->
      e >>| fun r -> (f, r) )
  >>| List.fold_left ~init ~f:(fun l (f, r) -> f l r)

module Make (M : Formula_parser.SPEC) =
struct
  type 'a t =
    | True
    | False
    | And of 'a t * 'a t
    | Or of 'a t * 'a t
    | Modal of 'a M.t
  [@@deriving sexp]

  let rec to_string formula ~to_string_parent =
    match formula with
    | True -> "true"
    | False -> "false"
    | And (a, b) ->
        let a_str = to_string ~to_string_parent a in
        let b_str = to_string ~to_string_parent b in
        {%string|%{a_str} && %{b_str}|}
    | Or (a, b) ->
        let a_str = to_string ~to_string_parent a in
        let b_str = to_string ~to_string_parent b in
        {%string|%{a_str} || %{b_str}|}
    | Modal body -> M.to_string body ~to_string_parent

  let parser ~formula =
    fix (fun closed_formula ->
        let word_atom =
          kw "true" *> return True
          <|> kw "tt" *> return True
          <|> kw "false" *> return False
          <|> kw "ff" *> return False
        in
        let parens = kw "(" *> closed_formula <* kw ")" in
        let atom =
          choice
            [
              (M.parser ~formula >>| fun m -> Modal m)
            ; parens
            ; kw "\u{22A4}" *> return True (* ⊤ *)
            ; kw "\u{22A5}" *> return False (* ⊥ *)
            ; word_atom
            ]
        in
        let and_ x y = And (x, y) in
        let and_op =
          (kw "&&" <|> kw "&" <|> kw "/\\" (* /\ *)
          <|> kw "\u{2227}" (* ∧ *))
          *> return and_
        in
        let or_ x y = Or (x, y) in
        let or_op =
          (kw "||" <|> kw "|" <|> kw "\\/" (* \/ *)
          <|> kw "\u{2228}" (* ∨ *))
          *> return or_
        in
        atom
        |> chain_binary ~op:and_op
        |> chain_binary ~op:or_op)

  let rec dual = function
    | True -> False
    | False -> True
    | And (a, b) -> Or (dual a, dual b)
    | Or (a, b) -> And (dual a, dual b)
    | Modal m -> Modal (M.dual m)

  let rec map ~f = function
    | True -> True
    | False -> False
    | And (a, b) -> And (map ~f a, map ~f b)
    | Or (a, b) -> Or (map ~f a, map ~f b)
    | Modal m -> Modal (M.map ~f m)
end
