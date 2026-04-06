open! Core
open Naive_modcheck_coalg_common
open Angstrom
open Lexer
(* ---- Chain combinators ---- *)

let chain_unary ~op e =
  fix (fun chain -> op <*> chain <|> e)

let chain_binary ~op e =
  e >>= fun init ->
  many
    ( op >>= fun f ->
      e >>| fun r -> (f, r) )
  >>| List.fold_left ~init ~f:(fun l (f, r) -> f l r)

(* ---- Generic formula parser (functor) ---- *)

module Make (M : Formula_ast.S) = struct
  let make_formula_parser
      ~(modal : M.t Angstrom.t -> M.t Angstrom.t) :
      string -> M.t =
    let resolve f =
      let rec go bound (ast : M.t) : M.t =
        match ast with
        | Ap name ->
            let s = Ap.to_string name in
            if Set.mem bound s then Var (Var.of_string s)
            else Ap name
        | Not f -> Not (go bound f)
        | And (a, b) -> And (go bound a, go bound b)
        | Or (a, b) -> Or (go bound a, go bound b)
        | Modal body -> Modal (M.modal_map (go bound) body)
        | Mu (x, body) ->
            Mu (x, go (Set.add bound (Var.to_string x)) body)
        | Nu (x, body) ->
            Nu (x, go (Set.add bound (Var.to_string x)) body)
        | (True | False | Var _) as f -> f
      in
      go String.Set.empty f
    in
    let parser =
      fix (fun formula ->
          let binder mk =
            (* Binds "X . f" part of mu/nu *)
            lift2
              (fun x body -> mk (Var.of_string x) body)
              word
              (kw "." *> formula)
          in
          let word_atom =
            word >>= fun word : M.t t ->
            match word with
            | "true"
            | "tt" ->
                return M.True
            | "false"
            | "ff" ->
                return M.False
            | "mu" -> binder (fun x body -> M.Mu (x, body))
            | "nu" -> binder (fun x body -> M.Nu (x, body))
            | s -> return (M.Ap (Ap.of_string s))
          in
          let parens = kw "(" *> formula <* kw ")" in
          let atom =
            choice
              [
                modal formula
              ; parens
              ; kw "\u{22A4}" *> return M.True (* ⊤ *)
              ; kw "\u{22A5}" *> return M.False (* ⊥ *)
              ; kw "\u{03BC}"
                *> binder (fun x f -> M.Mu (x, f))
                (* μ *)
              ; kw "\u{03BD}"
                *> binder (fun x f -> M.Nu (x, f))
                (* ν *)
              ; word_atom
              ]
          in
          let not_ x = M.Not x in
          let not_op =
            (kw "~" <|> kw "!" <|> kw "\u{00AC}") (* ¬ *)
            *> return not_
          in
          let and_ x y = M.And (x, y) in
          let and_op =
            (kw "&&"
             (* && has to be before &, otherwise the second & will not be eaten *)
            <|> kw "&" <|> kw "/\\" (* /\ *)
            <|> kw "\u{2227}" (* ∧ *))
            *> return and_
          in
          let or_ x y = M.Or (x, y) in
          let or_op =
            (kw "||" <|> kw "|" <|> kw "\\/" (* \/ *)
            <|> kw "\u{2228}" (* ∨ *))
            *> return or_
          in
          atom
          |> chain_unary ~op:not_op
          |> chain_binary ~op:and_op
          |> chain_binary ~op:or_op)
    in
    fun input ->
      match
        parse_string ~consume:Consume.All
          (spacing *> parser) input
      with
      | Ok f -> resolve f
      | Error msg ->
          failwith
            (Printf.sprintf "Formula parse error: %s" msg)
end
