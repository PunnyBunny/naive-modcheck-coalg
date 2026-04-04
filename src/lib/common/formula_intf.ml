open! Core

(** Module type for formulas parameterized by modality *)
module type S = sig
  type 'a modality [@@deriving sexp]

  val map_modality :
    ('a -> 'b) -> 'a modality -> 'b modality

  val negate_modality :
    ('a -> 'b) -> 'a modality -> 'b modality

  val iter_modality : ('a -> unit) -> 'a modality -> unit

  type t =
    | True
    | False
    | Ap of Ap.t
    | Not of Ap.t
    | Var of Var.t
    | And of t * t
    | Or of t * t
    | Modal of t modality
    | Mu of Var.t * t
    | Nu of Var.t * t
  [@@deriving sexp]

  val pretty_print : t -> string
end

(** Functor to create a formula module from a modality type
*)
module Make (M : sig
  type 'a t [@@deriving sexp]

  val to_string :
    'a t -> to_string_children:('a -> string) -> string

  val map : ('a -> 'b) -> 'a t -> 'b t
  val negate : ('a -> 'b) -> 'a t -> 'b t
end) : S with type 'a modality = 'a M.t = struct
  type 'a modality = 'a M.t [@@deriving sexp]

  let map_modality = M.map
  let negate_modality = M.negate
  let iter_modality f m = ignore (M.map f m)

  type t =
    | True
    | False
    | Ap of Ap.t
    | Not of Ap.t
    | Var of Var.t
    | And of t * t
    | Or of t * t
    | Modal of t modality
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
    | Modal f' ->
        M.to_string f' ~to_string_children:pretty_print
    | Mu (v, subfmla) ->
        "μ " ^ Var.to_string v ^ "." ^ pretty_print subfmla
    | Nu (v, subfmla) ->
        "ν " ^ Var.to_string v ^ "." ^ pretty_print subfmla
end
