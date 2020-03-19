(** [Short_paths_graph] is a representation of the environment (via the type
    [Graph.t]) that is more suitable to answer short path queries.

    The only structures shared with the typechecker are [Ident.t] and [Path.t].
    [Graph.t] is pure and doesn't hook into the [Env.t].
    Context has to be rebuilt by outside code using [Graph.add].

    For each item (a type, a class type, a module or a module type) the graph can:
    - lookup its definition from a path
    - resolve it to its canonical definition (following all aliases)
    - keep track of its origin (which modules were involved in its definition)
    - tell whether it is shadowed or not in a given context

    These functionalities alone do not give a short path, but they help
    computing it efficiently.
*)

(* Generic definitions *)

module String_map : Map.S with type key = string

module Ident : sig

  type t = Ident.t

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val name : t -> string

  val global : string -> t

end

module Ident_map : Map.S with type key = Ident.t

module Ident_set : Set.S with type elt = Ident.t

module Path : sig

  type t = Path.t =
    | Pident of Ident.t
    | Pdot of t * string * int
    | Papply of t * t

  val equal : t -> t -> bool

  val compare : t -> t -> int

end

module Path_map : Map.S with type key = Path.t

module Path_set : Set.S with type elt = Path.t

(* Subset of the type algebra that is relevant to short path.

   OCaml definitions are turned into short paths ones inside [Env]
   (see [Env.short_paths_type_desc] for instance).
   That way, [Short_path] is self-contained and works on a more appropriate
   representation.
*)

module Desc : sig

  module Type : sig

    (** Only the manifest part of a type is relevant to short-path.
        If it is a type constructor or a type variable, it is post-processed in
        one of the three cases below.
        Other cases (no manifest, not a type constructor or variable) cannot
        affect short-paths.
    *)
    type t =
      | Nth of int
      (** Manifest is the n'th type parameter.
          E.g. for n < m, [type ('x_0,'x_1,...,'x_m-1) t = 'x_n]
          is represented as [Nth n]. *)
      | Subst of Path.t * int list
      (** Manifest is an alias to some other type after substitution of type
          parameters.
          E.g. [type ('x_0, 'x_1', 'x_2, 'x_3) t = ('x_3, 'x_2) p]
          is represented as [Subst (p, [3,2])]. *)
      | Alias of Path.t
      (** Manifest is a direct alias to another type, preserving parameters.
          E.g [type t = p], [type 'a t = 'a p], ...
          are represented as [Alias p]. *)
      | Fresh
      (** Any other case. *)

  end

  (** [Class_type] and [Module_type] descriptions are similar to [Type].
      Cases that affect short-path are identified, other case are mapped to
      [Fresh]. *)

  module Class_type : sig

    type t =
      | Subst of Path.t * int list
      (** [class type ['a,'b] t = ['b, 'a] u] *)
      | Alias of Path.t
      (** [class type t = u] or [class type ['a] t = ['a] u] *)
      | Fresh
      (** Any other case. *)

  end

  module Module_type : sig

    type t =
      | Alias of Path.t
      (** [module type T = U] *)
      | Fresh
      (** Any other case. *)

  end

  (** [Module] case is a bit more delicate.
      The contents of a module has to be considered too for short-paths. *)

  module Module : sig

    type component =
      | Type of string * Type.t
      (** [type name = <description>] *)
      | Class_type of string * Class_type.t
      (** [class type name = <description>] *)
      | Module_type of string * Module_type.t
      (** [module type name = <description>] *)
      | Module of string * t
      (** [module name = <description>] *)

    and components = component list
    (** a signature is a list of component *)

    and kind =
      | Signature of components Lazy.t
      (** The contents of a signature are computed lazily, this allows to skip
          a significant amount of computation. *)
      | Functor of (Path.t -> t)
      (** "HOAS" encoding of functors.
          In [Functor f], [f path_X] will give the module that results from
          applying the functor to [X].
          Consumer of this API don't have to deal with substitution manually
          and variables don't have to be represented explicitly.
          (however, not all [(Path.t -> t)] functions are admissible).  *)

    and t =
      | Alias of Path.t
      (** Alias to an existing module, [module U = V]. *)
      | Fresh of kind
      (** In other cases, the contents of the module is described *)

  end

  (** Source of environment entries.  *)
  type source =
    | Local (** entry comes from a local definition *)
    | Open (** entry comes from the contents of an [open Module] item *)

  (** Description of environment entries.  *)
  type t =
    | Type of
        { ident: Ident.t; desc: Type.t; source: source }
    | Class_type of
        { ident: Ident.t; desc: Class_type.t; source: source }
    | Module_type of
        { ident: Ident.t; desc: Module_type.t; source: source }
    | Module of
        { ident: Ident.t; desc: Module.t; source: source }

end

(** [Age] defines when an entry became available.

    Somewhat counter-intuitively [Age] actually represents a point in time:
    - global definitions have age [Age.zero]
    - each time an environment is extended, the age is incremented.
    So older entries have lower [Age], most recent entries have the highest
    [Age].

    While [Age] values are stored in the [Graph], they are always introduced by
    [Short_paths].
    [Short_path_graphs] only ensure that the proper [Age] is computed for
    results of functor applications.
*)
module Age : Natural.S

(** Each global module referenced by the environment is given a [Dependency.t]
    number.  This also applies to modules that are aliased but not used (hence
    not loaded).

    The number itself has no semantic meaning ([Dependency] is an
    instance of [Natural.S] just to have an infinite set of values to pick
    from, it is used as a gensym).

    Dependency numbers are attributed during [Basis] update.
    Each time a global module is loaded, [Env] will append the module to the
    [Basis].
    The updates are done lazily: next time the [Basis] is queried, dependency
    numbers will be chosen.
*)
module Dependency : Natural.S

(** The [Origin] of an item tells when it became well-defined.
    If the item depends only on global modules, its origin will list these
    modules.
    If the item depends on some local definitions, then its origin will be the
    smallest [Age] at which all these definitions are available.
*)
module Origin : sig

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
    | Environment of Age.t
    (** Item originates from local environment, at a specific [Age]. *)

  val equal : t -> t -> bool

  val hash : t -> int

end

module Sort : sig

  (* CR lwhite: These descriptions are not accurate *)
  (** Each item (type, module, class type, ...) comes with a sort.
      The sorts are used to compute updates in [Short_paths.Forward_path_map].
  *)
  type t =
    | Defined
    (** Environment entries (a top-level [type t]) have sort [Defined]. *)
    | Unloaded of Dependency.t
    (** Sub-entries of modules (such as [String.t]) have sort [Unloaded ids],
        where [ids] is the set of identifiers of modules that participate in
        the declaration of the entry.
        In [String.t] it is the singleton [String], but it can grow in entries
        that result from functor applications.  In [Map.Make(String).t] the
        identifiers are {[String], [Map]}. *)
end

module Component : sig

  type source =
    | Global
    (** Component comes from a global import. Only applicable to modules. *)
    | Local
    (** Component comes from a local definition. *)
    | Open
    (** Component comes from an open item. *)

  (** A [Component.t] augments a [Desc.t] with an origin and more
      detailed source. *)
  type t =
    | Type of Origin.t * Ident.t * Desc.Type.t * source
    | Class_type of Origin.t * Ident.t * Desc.Class_type.t * source
    | Module_type of Origin.t * Ident.t * Desc.Module_type.t * source
    | Module of Origin.t * Ident.t * Desc.Module.t * source
    | Declare_module of Origin.t * Ident.t

end

(** The [graph] is the short-path specific representation of an environment.
    It aggregates components and do name resolution.

    Resolution turns syntactic descriptions into abstract values:
    - Desc.Type.t        -> Type.t
    - Desc.Class_type.t  -> Class_type.t
    - Desc.Module.t      -> Module.t
    - Desc.Module_type.t -> Module_type.t
*)

type graph

(* Abstract definitions for each item.
   [origin], [path] and [sort] are available for all kinds of item.
*)

module Type : sig

  type t

  val origin : graph -> t -> Origin.t

  val path : graph -> t -> Path.t

  val sort : graph -> t -> Sort.t

  (** Reminiscent of [Desc.Type.t] *)
  type resolved =
    | Nth of int
    (** type is projection of n'th parameter *)
    | Path of int list option * t
    (** [Path (None, t)] =>
          type is an alias to (or is) [t].
        [Path (Some params, t)] =>
          type is an alias to [t] with substituted parameters.
    *)

  val resolve : graph -> t -> resolved

end

module Class_type : sig

  type t

  val origin : graph -> t -> Origin.t

  val path : graph -> t -> Path.t

  val sort : graph -> t -> Sort.t

  type resolved = int list option * t

  val resolve : graph -> t -> resolved

end

module Module_type : sig

  type t

  val origin : graph -> t -> Origin.t

  val path : graph -> t -> Path.t

  val sort : graph -> t -> Sort.t

end

(** For a module, entries of each kind can be additionnally queried. *)

module Module : sig

  type t

  val origin : graph -> t -> Origin.t

  val path : graph -> t -> Path.t

  val sort : graph -> t -> Sort.t

  (** Entries are [None] when a module is not a structure (a functor...). *)

  val types : graph -> t -> Type.t String_map.t option

  val class_types : graph -> t -> Class_type.t String_map.t option

  val module_types : graph -> t -> Module_type.t String_map.t option

  val modules : graph -> t -> t String_map.t option

end

(** Diff and rebase.

    Environments (Env.t) are made of two parts: basis (a set of global modules)
    and local definitions.

    Modules in the basis are loaded on demand. When a [Graph.t] is made from an
    [Env.t], the current basis is used.

    Since the graph is pure, the [Env.t] and the graph will get out of sync as
    new modules are loaded. This is solved using [Diff.t] and [Graph.rebase]:
    - the [Graph.t] made from the initial environment is remembered
    - local definitions are made by extending the graph with [Graph.add]
    - updates to the basis are introduced with [Graph.add] starting again from
      the initial graph, which also produces a [Diff.t]
    - these differences can be injected into the extended [Graph.t] by using
      [Graph.rebase] to replay a set of [Diff.t]

    The main difference between a [Graph.rebase]/[Diff.t] and just adding
    everything on top with [Graph.add] is that shadowing is taken into account.
    [Graph.rebase] will insert definitions at the right point in "time".
*)

module Diff : sig

  module Item : sig

    type t =
      | Type of Ident.t * Type.t * Origin.t option
      | Class_type of Ident.t * Class_type.t * Origin.t option
      | Module_type of Ident.t * Module_type.t * Origin.t option
      | Module of Ident.t * Module.t * Origin.t option

    val origin : graph -> t -> Origin.t

    val id : graph -> t -> Ident.t

    val previous : graph -> t -> Origin.t option

  end

  type t = Item.t list

end

module Graph : sig

  type t = graph

  val empty : t

  val add : t -> Component.t list -> t * Diff.t

  val rebase : t -> Diff.t -> t

  val find_type : t -> Path.t -> Type.t

  val find_class_type : t -> Path.t -> Class_type.t

  val find_module_type : t -> Path.t -> Module_type.t

  val find_module : t -> Path.t -> Module.t

  val is_type_path_visible : t -> Path.t -> bool

  val is_class_type_path_visible : t -> Path.t -> bool

  val is_module_type_path_visible : t -> Path.t -> bool

  val is_module_path_visible : t -> Path.t -> bool

end
