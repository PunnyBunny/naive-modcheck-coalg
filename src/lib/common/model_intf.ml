open! Core

(** Module type for models parameterized by transition type
*)
module type S = sig
  type transition [@@deriving sexp]

  type t =
    ( State.t
    , Ap.t list * (Action.t, transition) Hashtbl.Poly.t )
    Hashtbl.Poly.t
  [@@deriving sexp]

  val pretty_print : t -> string
end

(** Functor to create a model module from a transition type
*)
module Make (T : sig
  type t [@@deriving sexp]

  val to_string : t -> string
end) : S with type transition = T.t = struct
  type transition = T.t [@@deriving sexp]

  type t =
    ( State.t
    , Ap.t list * (Action.t, transition) Hashtbl.Poly.t )
    Hashtbl.Poly.t
  [@@deriving sexp]

  let pretty_print model =
    let pp_list pp_elem l =
      let inner =
        String.concat ~sep:", " (List.map ~f:pp_elem l)
      in
      {%string|{%{inner}}|}
    in
    let pp_action a =
      if Action.is_empty a then "{}" else Action.to_string a
    in
    let pp_func entries =
      let inner = String.concat ~sep:", " entries in
      {%string|[%{inner}]|}
    in
    let entries =
      Hashtbl.Poly.fold model ~init:[]
        ~f:(fun ~key:state ~data:(aps, transitions) acc ->
          let transition_entries =
            Hashtbl.Poly.fold transitions ~init:[]
              ~f:(fun ~key:action ~data:transition acc ->
                let action = pp_action action in
                let transition = T.to_string transition in
                {%string|%{action}: %{transition}|} :: acc)
          in
          let aps = pp_list Ap.to_string aps in
          let transitions = pp_func transition_entries in
          let state = State.to_string state in
          {%string|%{state}: (%{aps}, %{transitions})|}
          :: acc)
    in
    pp_func entries
end
