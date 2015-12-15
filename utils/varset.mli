
type id = int

type 'a t

val empty: 'a t

val is_empty: 'a t -> bool

val mem: id -> 'a t -> bool

val add: id -> 'a -> 'a t -> 'a t

val singleton: id -> 'a -> 'a t

val remove: id -> 'a t -> 'a t

val union: 'a t -> 'a t -> 'a t

val inter: 'a t -> 'a t -> 'a t

val diff: 'a t -> 'a t -> 'a t

val compare: 'a t -> 'a t -> int

val equal: 'a t -> 'a t -> bool

val subset: 'a t -> 'a t -> bool

val iter: ('a -> unit) -> 'a t -> unit

val fold: ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b

val for_all: ('a -> bool) -> 'a t -> bool

val exists: ('a -> bool) -> 'a t -> bool

val filter: ('a -> bool) -> 'a t -> 'a t

val partition: ('a -> bool) -> 'a t -> 'a t * 'a t

val cardinal: 'a t -> int

val elements: 'a t -> 'a list

val min_elt: 'a t -> 'a

val max_elt: 'a t -> 'a

val choose: 'a t -> 'a

val split: id -> 'a t -> 'a t * bool * 'a t

val find: id -> 'a t -> 'a
