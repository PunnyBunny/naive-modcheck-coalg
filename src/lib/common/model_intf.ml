open! Core

(** Module type for models parameterized by transition type
*)
module type S = sig
  type 'a t' [@@deriving sexp]
  (** Functor for the coalgebra *)

  type t = (State.t, State.t t') Hashtbl.Poly.t
  [@@deriving sexp]

  val states : t -> State.t list
  val pretty_print : t -> string
end

(* module type MODEL_FUNCTOR_SPECIFICATION = sig
  type 'a t [@@deriving sexp]

  val to_string :
    'a t -> to_string_parent:('a -> string) -> string
end *)

(** Functor to create a model module from a transition type
*)
(* module Make (T : MODEL_FUNCTOR_SPECIFICATION) :
  S with type 'a t' = 'a T.t = struct
  type 'a t' = 'a T.t [@@deriving sexp]

  type t = (State.t, State.t T.t) Hashtbl.Poly.t
  [@@deriving sexp]

  let states model = Hashtbl.keys model

  let pretty_print tbl =
    let entries =
      Hashtbl.Poly.fold tbl ~init:[]
        ~f:(fun ~key:state ~data:succ acc ->
          let state_str = State.to_string state in
          let succ_str =
            T.to_string succ
              ~to_string_parent:State.to_string
          in
          {%string|%{state_str}: %{succ_str}|} :: acc)
    in
    let inner = String.concat ~sep:", " entries in
    {%string|[%{inner}]|}
end *)
