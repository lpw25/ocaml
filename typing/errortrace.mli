open Types

type position = First | Second

type desc = { t: type_expr; expanded: type_expr option }
type 'a diff = { got: 'a; expected: 'a}

(** [map_diff f {expected;got}] is [{expected=f expected; got=f got}] *)
val map_diff: ('a -> 'b) -> 'a diff -> 'b diff

(** Scope escape related errors *)
type 'a escape_kind =
  | Constructor of Path.t
  | Univ of type_expr
  (* The type_expr argument of [Univ] is always a [Tunivar _],
     we keep a [type_expr] to track renaming in {!Printtyp} *)
  | Self
  | Module_type of Path.t
  | Equation of 'a
  | Constraint

type 'a escape =
  { kind : 'a escape_kind;
    context : type_expr option }

val short : type_expr -> desc

val explain: 'a list ->
  (prev:'a option -> 'a -> 'b option) ->
  'b option

module Unification: sig
  type fixed_row_case =
    | Cannot_be_closed
    | Cannot_add_tags of string list

  type variant =
    | No_intersection
    | No_tags of position * (Asttypes.label * row_field) list
    | Incompatible_types_for of string
    | Fixed_row of position * fixed_row_case * fixed_explanation

  type obj =
    | Missing_field of position * string
    | Abstract_row of position
    | Self_cannot_be_closed

  type 'a elt =
    | Diff of 'a diff
    | Variant of variant
    | Obj of obj
    | Escape of 'a escape
    | Incompatible_fields of {name:string; diff: type_expr diff }
    | Rec_occur of type_expr * type_expr

  type t = desc elt list

  val diff: type_expr -> type_expr -> desc elt

  (** [flatten f trace] flattens all elements of type {!desc} in
      [trace] to either [f x.t expanded] if [x.expanded=Some expanded]
      or [f x.t x.t] otherwise *)
  val flatten: (type_expr -> type_expr -> 'a) -> t -> 'a elt list

  (** Switch [expected] and [got] *)
  val swap: t -> t

  val map : (desc -> desc) -> desc elt list -> desc elt list

  val incompatible_fields : string -> type_expr -> type_expr -> desc elt

end

module Equality: sig
  type variant =
    | Incompatible_types_for of string
    | Openness of position
    | Missing of (position * Asttypes.label)

  type obj =
    | Missing_field of position * string
    | Abstract_row of position

  type 'a elt =
    | Diff of 'a diff
    | Variant of variant
    | Obj of obj
    | Escape of 'a escape
    | Incompatible_fields of {name:string; diff: type_expr diff }

  type t = desc elt list

  val diff: type_expr -> type_expr -> desc elt

  (** [flatten f trace] flattens all elements of type {!desc} in
      [trace] to either [f x.t expanded] if [x.expanded=Some expanded]
      or [f x.t x.t] otherwise *)
  val flatten: (type_expr -> type_expr -> 'a) -> t -> 'a elt list

  val map : (desc -> desc) -> desc elt list -> desc elt list

  val incompatible_fields : string -> type_expr -> type_expr -> desc elt
end

module Moregen : sig
  type variant =
    | Missing of position * Asttypes.label
    | Openness
    | Incompatible_types_for of string

  type obj =
    | Missing_field of position * string
    | Abstract_row of position
    (* | Self_cannot_be_closed *)

  type 'a elt =
    | Diff of 'a diff
    | Variant of variant
    | Obj of obj
    | Escape of 'a escape
    | Incompatible_fields of {name:string; diff: type_expr diff }
    | Rec_occur of type_expr * type_expr

  type t = desc elt list

  val diff: type_expr -> type_expr -> desc elt

  val map : (desc -> desc) -> desc elt list -> desc elt list

  (** [flatten f trace] flattens all elements of type {!desc} in
      [trace] to either [f x.t expanded] if [x.expanded=Some expanded]
      or [f x.t x.t] otherwise *)
  val flatten: (type_expr -> type_expr -> 'a) -> t -> 'a elt list

  val incompatible_fields : string -> type_expr -> type_expr -> desc elt
end

module Subtype : sig
  type 'a elt =
    | Diff of 'a diff

  type t = desc elt list

  val diff: type_expr -> type_expr -> desc elt

  val map : (desc -> desc) -> desc elt list -> desc elt list

  val flatten: (type_expr -> type_expr -> 'a) -> t -> 'a elt list

end
