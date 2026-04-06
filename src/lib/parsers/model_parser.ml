open! Core
open Naive_modcheck_coalg_common
open Angstrom
open Lexer

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

module Make (M : Model_intf.S) = struct
  let make_model_parser
      ~(transition : M.transition Angstrom.t) :
      string -> M.t =
    let state_data =
      kw "("
      *> lift2
           (fun aps trans -> (aps, trans))
           (list_of ap <* kw ",")
           (func_of action transition)
      <* kw ")"
    in
    let entry =
      lift2 (fun s d -> (s, d)) (state <* kw ":") state_data
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
