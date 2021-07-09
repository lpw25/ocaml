(** [Short_paths_loads.Basis.t] is a representation of the persistent
    part of the environment. It tracks which compilation units have been
    loaded and the reverse dependencies of these units. *)

module String_set : Set.S with type elt = string

module String_map : Map.S with type key = string

(** Each global module referenced by the environment is given a [Dependency.t]
    number.  This also applies to modules that are aliased but not used (hence
    not loaded).

    The number itself has no semantic meaning ([Dependency] is an
    instance of [Natural.S] just to have an infinite set of values to pick
    from (it is used as a gensym).

    Dependency numbers are attributed during [Basis] update.
    Each time a global module is loaded, [Env] will append the module to the
    [Basis].
    The updates are done lazily: next time the [Basis] is queried, dependency
    numbers will be chosen.
*)
module Dependency : Natural.S

(** [Time] defines a point in the history of the (non-persistent) environment:
    - global definitions have age [Time.zero]
    - each time an environment is extended, the age is incremented.

    Whilst [Time] values are handled by some functions in
    [Short_paths_loads] and stored in the graph in [Short_paths_graph],
    it is [Short_paths] that always creates them and is responsible for
    manging them.
*)
module Time : Natural.S

(** The [Origin] of an item tells when it became well-defined.  For
    simple paths this is the point at which the underlying identifier
    was defined: either as a global dependency or a [Time.t] in the
    local environment. Extended paths can depend on multiple identifiers
    and so may be a list of global dependencies. *)
module Origin : sig

  type t =
    | Dependency of Dependency.t
    (** Item originates from a global dependency. *)
    | Environment of Time.t
    (** Item originates from local environment, at a specific [Time]. *)

  val equal : t -> t -> bool

  val hash : t -> int

  (** The origin of an extended path (i.e. one including functor
      applications) If the item depends only on global modules, its
      origin will list these modules. If the item depends on some local
      definitions, then its origin will be the earliest [Time] at which
      all these definitions are available. *)
  module Extended : sig

    type t =
      | Dependency of Dependency.t
      (** Item originates from a global dependency. *)
      | Dependencies of Dependency.t list
      (** Item originates from more than one global dependency.  This is possible
          with functor applications: items from [Map.Make(String)] will have
          [Dependencies [Map; String]].
          The list is sorted in increasing order (it is actually a set).

          While [Dependency d] should behave exactly like [Dependencies [d]],
          this case is distinguished for performance.
      *)
      | Environment of Time.t
      (** Item originates from local environment, at a specific [Time]. *)

    val equal : t -> t -> bool

    val hash : t -> int

    (** The origin of a functor application given the origins
        of its functor and argument *)
    val application : t -> t -> t

  end

end

(** [Rev_deps] keeps track of the reverse dependencies of each global module
    (represented as a [Dependency.t]).  *)
module Rev_deps : sig

  type t

  (** Get the transitive set of reverse dependencies of a module
      (note: a module is part of its own reverse dependencies).  *)
  val get : t -> Dependency.t -> Dependency.Set.t

  (** [before t o1 o2] is true if [o2] exists only in environments where
      [o1] has already been defined.  This is the case if [o2] is a
      local definition made after [o1], or if [o2] comes from a global
      module that depends on [o1].

      Thus, [before] defines a total ordering on local definitions and a
      partial ordering on global dependencies. *)
  val before : t -> Origin.t -> Origin.t -> bool

end

(** [History.Revision.t] represents a point in the history of
    the persistent environment. *)
module History : sig

  module Stamp : Natural.S

  module Revision : sig

    type t

    val stamp : t -> Stamp.t

    val loads : t -> Dependency.t list

    val next : t -> t option

  end

end

module Load : sig

  type t =
    { name : string;
      depends : string list;
      alias_depends : string list; }

end

module Basis : sig

  type t

  val create : unit -> t

  val add : t -> references:String_set.t -> loads:Load.t list -> unit

  val head : t -> History.Revision.t

  val rev_deps : t -> Rev_deps.t

  val find_dependency : t -> string -> Dependency.t

end
