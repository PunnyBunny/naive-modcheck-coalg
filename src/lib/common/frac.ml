open! Core

type t = Q.t

let make n d = Q.make (Z.of_int n) (Z.of_int d)
let zero = Q.zero
let of_int = Q.of_int

let sexp_of_t q =
  let n = Z.to_int (Q.num q) in
  let d = Z.to_int (Q.den q) in
  Sexp.List
    [
      Sexp.Atom (Int.to_string n)
    ; Sexp.Atom (Int.to_string d)
    ]

let t_of_sexp sexp =
  match sexp with
  | Sexp.List [ Sexp.Atom n; Sexp.Atom d ] ->
      make (Int.of_string n) (Int.of_string d)
  | _ -> failwith "Frac.t_of_sexp: expected (n d)"

let compare = Q.compare
let ( + ) = Q.add
let ( >= ) a b = Q.compare a b >= 0
let ( > ) a b = Q.compare a b > 0
let to_string = Q.to_string
