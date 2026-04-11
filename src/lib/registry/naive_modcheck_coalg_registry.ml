open! Core
open Naive_modcheck_coalg_checker

(** A GADT-based existential wrapper around a logic +
    checker pair.

    The [Packed] constructor hides the logic-specific model,
    formula, and modality types behind an existential so
    that callers can work with any logic uniformly. Unlike a
    plain closure, the first-class module is preserved —
    callers who pattern-match get access to every function
    in {!Checker_intf.S} (parse, convert, check, …) and can
    compose them however they like. *)
type packed_logic =
  | Packed :
      string * (module Checker_intf.S)
      -> packed_logic

(** Extract the name from a packed logic. *)
let name (Packed (n, _)) = n

(** Run the full model-checking pipeline for a packed logic:
    parse model → parse formula → convert to NNF → check. *)
let run (Packed (_, (module C))) ~verbose ~model_src
    ~formula_src ~point =
  let model = C.Logic.parse_model model_src in
  let formula = C.Logic.parse_formula formula_src in
  C.model_check_full ~verbose ~model ~point ~formula

(* ── Built-in logics ──────────────────────────────────────────────────── *)

let logics : packed_logic list =
  [
    Packed ("relational", (module Checkers.Relational))
  ; Packed ("probabilistic", (module Checkers.Probabilistic))
  ]

(** Look up a logic by name (case-insensitive). *)
let find_logic query =
  List.find logics ~f:(fun packed ->
      String.Caseless.equal (name packed) query)

(** All registered logic names, in registration order. *)
let known_names () = List.map logics ~f:name
