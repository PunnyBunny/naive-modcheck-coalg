open! Core
open Angstrom
open Naive_modcheck_coalg_common
open Naive_modcheck_coalg_parsers_common.Lexer

(* ---- Model-specific tokens ---- *)

let ident = word
let state = ident >>| State.of_string
let ap = ident >>| Ap.of_string

let action =
  kw "{" *> kw "}"
  >>| (fun _ -> Action.of_string "")
  <|> (ident >>| Action.of_string)

let frac = lift2 Frac.make integer (kw "/" *> integer)

(* ---- Grammar combinators ---- *)

let list_of p = kw "{" *> sep_by (kw ",") p <* kw "}"

let func_of key val_ =
  let entry =
    lift2 (fun k v -> (k, v)) (key <* kw ":") val_
  in
  kw "[" *> sep_by (kw ",") entry <* kw "]" >>| fun pairs ->
  let tbl = Hashtbl.Poly.create () in
  List.iter pairs ~f:(fun (k, v) ->
      Hashtbl.add_exn tbl ~key:k ~data:v);
  tbl

(* ---- Generic model parser (functor) ---- *)

module type SPEC = sig
  type 'a t [@@deriving sexp]

  val to_string :
    'a t -> to_string_inner:('a -> string) -> string

  val parser : model_inner:'a Angstrom.t -> 'a t Angstrom.t
end

module Make (M : SPEC) = struct
  type 'a transition = 'a M.t [@@deriving sexp]

  type t = (State.t, State.t M.t) Hashtbl.Poly.t
  [@@deriving sexp]

  let states model = Hashtbl.keys model

  let pretty_print tbl =
    let entries =
      Hashtbl.Poly.fold tbl ~init:[]
        ~f:(fun ~key:state ~data:succ acc ->
          let state_str = State.to_string state in
          let succ_str =
            M.to_string succ
              ~to_string_inner:State.to_string
          in
          {%string|%{state_str}: %{succ_str}|} :: acc)
    in
    let inner = String.concat ~sep:", " entries in
    {%string|[%{inner}]|}

  let parse_model =
    let entry =
      lift2
        (fun s d -> (s, d))
        (state <* kw ":")
        (M.parser ~model_inner:state)
    in
    let bracketed =
      kw "[" *> sep_by (kw ",") entry <* kw "]"
    in
    let unbracketed = many entry in
    let model =
      bracketed <|> unbracketed >>| fun pairs ->
      let tbl = Hashtbl.Poly.create () in
      List.iter pairs ~f:(fun (s, d) ->
          Hashtbl.add_exn tbl ~key:s ~data:d);
      tbl
    in
    fun input ->
      match
        parse_string ~consume:Consume.All (spacing *> model)
          input
      with
      | Ok m -> m
      | Error e ->
          failwith
            (Printf.sprintf "Model parse error: %s" e)
end
