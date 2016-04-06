
type t

val root : t -> string option

val parents : t -> string list

val name : t -> string

val dummy : t

val simple : name:string -> t

val relative : parents:string list -> name:string -> t

val absolute : root:string -> parents:string list -> name:string -> t

val equal : t -> t -> bool

val compare : t -> t -> int

val hash : t -> int

val print : Format.formatter -> t -> unit

module Set : Set.S with type elt = t

module Tbl : Hashtbl.S with type key = t
