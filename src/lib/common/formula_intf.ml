open! Core

(** Module type for formulas parameterized by modality *)
module type S = sig
  type modality [@@deriving sexp]

  type t =
    | True
    | False
    | Ap of Ap.t
    | Not of Ap.t
    | Var of Var.t
    | And of t * t
    | Or of t * t
    | Diamond of Action.t * modality * t
    | Box of Action.t * modality * t
    | Mu of Var.t * t
    | Nu of Var.t * t
  [@@deriving sexp]

  val pretty_print : t -> string
end

(** Functor to create a formula module from a modality type
*)
module Make (M : sig
  type t [@@deriving sexp]

  val to_string : t -> string
end) : S with type modality = M.t = struct
  type modality = M.t [@@deriving sexp]

  type t =
    | True
    | False
    | Ap of Ap.t
    | Not of Ap.t
    | Var of Var.t
    | And of t * t
    | Or of t * t
    | Diamond of Action.t * modality * t
    | Box of Action.t * modality * t
    | Mu of Var.t * t
    | Nu of Var.t * t
  [@@deriving sexp]

  let rec pretty_print formula =
    match formula with
    | True -> "⊤"
    | False -> "⊥"
    | Ap atom -> Ap.to_string atom
    | Not atom -> "¬" ^ Ap.to_string atom
    | Var v -> Var.to_string v
    | And (f1, f2) ->
        "(" ^ pretty_print f1 ^ " ∧ " ^ pretty_print f2
        ^ ")"
    | Or (f1, f2) ->
        "(" ^ pretty_print f1 ^ " ∨ " ^ pretty_print f2
        ^ ")"
    | Diamond (action, modality, subfmla) ->
        "<"
        ^ (if Action.to_string action |> String.is_empty
           then ""
           else Action.to_string action ^ ", ")
        ^ M.to_string modality
        ^ ">"
        ^ pretty_print subfmla
    | Box (action, modality, subfmla) ->
        "["
        ^ (if Action.to_string action |> String.is_empty
           then ""
           else Action.to_string action ^ ", ")
        ^ M.to_string modality
        ^ "]"
        ^ pretty_print subfmla
    | Mu (v, subfmla) ->
        "μ " ^ Var.to_string v ^ "." ^ pretty_print subfmla
    | Nu (v, subfmla) ->
        "ν " ^ Var.to_string v ^ "." ^ pretty_print subfmla
end
