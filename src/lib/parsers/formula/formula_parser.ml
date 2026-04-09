open! Core
open Naive_modcheck_coalg_common
open Angstrom
open Naive_modcheck_coalg_parsers_common.Lexer
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

module type SPEC = sig
  type 'a t [@@deriving sexp]

  val to_string :
    'a t -> to_string_parent:('a -> string) -> string

  val parser : formula:'a Angstrom.t -> 'a t Angstrom.t
  val dual : 'a t -> 'a t
  val map : f:('a -> 'b) -> 'a t -> 'b t
end

module Make (M : SPEC) = struct
  type t =
    | True
    | False
    | Var of Var.t
    | And of t * t
    | Or of t * t
    | Modal of t M.t
    | Mu of Var.t * t
    | Nu of Var.t * t
  [@@deriving sexp]

  type t' =
    | Not of t'
    | True'
    | False'
    | Var' of Var.t
    | And' of t' * t'
    | Or' of t' * t'
    | Modal' of t' M.t
    | Mu' of Var.t * t'
    | Nu' of Var.t * t'
  [@@deriving sexp]

  let parse_formula : string -> t =
    let parser_with_negation =
      fix (fun formula ->
          let binder mk =
            (* Binds "X . f" part of mu/nu *)
            lift2
              (fun x body -> mk (Var.of_string x) body)
              word
              (kw "." *> formula)
          in
          let word_atom =
            word >>= function
            | "true"
            | "tt" ->
                return True'
            | "false"
            | "ff" ->
                return False'
            | "mu" -> binder (fun x body -> Mu' (x, body))
            | "nu" -> binder (fun x body -> Nu' (x, body))
            | s -> return (Var' (Var.of_string s))
          in
          let parens = kw "(" *> formula <* kw ")" in
          let atom =
            choice
              [
                (M.parser ~formula >>| fun m -> Modal' m)
              ; parens
              ; kw "\u{22A4}" *> return True' (* ⊤ *)
              ; kw "\u{22A5}" *> return False' (* ⊥ *)
              ; kw "\u{03BC}"
                *> binder (fun x f -> Mu' (x, f))
                (* μ *)
              ; kw "\u{03BD}"
                *> binder (fun x f -> Nu' (x, f))
                (* ν *)
              ; word_atom
              ]
          in
          let not_ x = Not x in
          let not_op =
            (kw "~" <|> kw "!" <|> kw "\u{00AC}") (* ¬ *)
            *> return not_
          in
          let and_ x y = And' (x, y) in
          let and_op =
            (kw "&&"
             (* && has to be before &, otherwise the second & will not be eaten *)
            <|> kw "&" <|> kw "/\\" (* /\ *)
            <|> kw "\u{2227}" (* ∧ *))
            *> return and_
          in
          let or_ x y = Or' (x, y) in
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
    let convert_to_nnf =
      let rec negate = function
        | Not f -> f
        | True' -> False'
        | False' -> True'
        | Var' v -> Var' v
        | And' (f, g) -> Or' (negate f, negate g)
        | Or' (f, g) -> And' (negate f, negate g)
        | Modal' m -> Modal' (M.map ~f:negate (M.dual m))
        | Mu' (x, f) -> Nu' (x, negate f)
        | Nu' (x, f) -> Mu' (x, negate f)
      in
      let rec to_nnf = function
        | Not f -> to_nnf (negate f)
        | True' -> True
        | False' -> False
        | Var' v -> Var v
        | And' (f, g) -> And (to_nnf f, to_nnf g)
        | Or' (f, g) -> Or (to_nnf f, to_nnf g)
        | Modal' m -> Modal (M.map ~f:to_nnf m)
        | Mu' (x, f) -> Mu (x, to_nnf f)
        | Nu' (x, f) -> Nu (x, to_nnf f)
      in
      to_nnf
    in
    fun input ->
      match
        parse_string ~consume:Consume.All
          (spacing *> parser_with_negation)
          input
      with
      | Ok f -> convert_to_nnf f
      | Error msg ->
          failwith
            (Printf.sprintf "Formula parse error: %s" msg)
end
