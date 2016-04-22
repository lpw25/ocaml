
type t

val root : t -> string option

val name : t -> string

val dummy : t

val simple : name:string -> t

val base : root:string -> name:string -> t

val project : parent:t -> name:string -> t

val project_or_simple : parent:t -> name:string -> t

val equal : t -> t -> bool

val compare : t -> t -> int

val hash : t -> int

val print : Format.formatter -> t -> unit

module Set : Set.S with type elt = t

module Tbl : Hashtbl.S with type key = t
