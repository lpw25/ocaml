(** [Short_paths_graph] is a representation of the environment (as a graph,
    using [Graph.t]) that is more suitable to answer short path queries.

    The only structures shared with the typechecker are [Ident.t] and [Path.t].
    [Graph.t] is pure and doesn't hook into the [Env.t].
    Context has to be rebuilt by outside code using [Graph.add].
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
   That way, [Short_path] is self contained and work on a specialy crafted
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

  (* CR def for lpw25: consider using inline records to make parameters more
     intelligible. *)

  (** Description of environment entries.
      The [bool] parameter of each constructor indicates whether
      the entry comes from a local definition (when [true]) or an open item
      (when [false]).  *)
  type t =
    | Type of Ident.t * Type.t * bool
    | Class_type of Ident.t * Class_type.t * bool
    | Module_type of Ident.t * Module_type.t * bool
    | Module of Ident.t * Module.t * bool

end

module Sort : sig

  (** Each item (type, module, class type, ...) comes with a sort.
      The sorts are used to compute updates in [Short_paths.Forward_path_map].
  *)
  type t =
    | Defined
    (** Environment entries (a top-level [type t]) have sort [Defined]. *)
    | Declared of Ident_set.t
    (** Sub-entries of modules (such as [String.t]) have sort [Declared ids],
        where [ids] is the set of identifiers of modules that participate in
        the declaration of the entry.
        In [String.t] it is the singleton [String], but it can grow in entries
        that result from functor applications.  In [Map.Make(String).t] the
        identifiers are {[String], [Map]}. *)
end

(** CR def for lpw25: unsure about that.
    [Age] defines when an entry became available.

    Somewhat counter-intuitively [Age] actually represents a point in time:
    - global definitions have age [Age.zero]
    - everytime an environment is extended, the age is incremented.
    So older entries have lower [Age], most recent entries have the highest
    [Age].

    While [Age] values are stored in the [Graph], they are always introduced by
    [Short_paths].
    [Short_path_graphs] only ensure that the right [Age] is computed
*)
module Age : Natural.S

module Dependency : Natural.S

module Origin : sig

  type t =
    | Dependency of Dependency.t
    | Dependencies of Dependency.t list
    | Environment of Age.t

  val equal : t -> t -> bool

  val hash : t -> int

end

type graph

module Type : sig

  type t

  val origin : graph -> t -> Origin.t

  val path : graph -> t -> Path.t

  val sort : graph -> t -> Sort.t

  type resolved =
    | Nth of int
    | Path of int list option * t

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

module Module : sig

  type t

  val origin : graph -> t -> Origin.t

  val path : graph -> t -> Path.t

  val sort : graph -> t -> Sort.t

  val types : graph -> t -> Type.t String_map.t option

  val class_types : graph -> t -> Class_type.t String_map.t option

  val module_types : graph -> t -> Module_type.t String_map.t option

  val modules : graph -> t -> t String_map.t option

end

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

module Component : sig

  type source =
    | Global
    | Local
    | Open

  type t =
    | Type of Origin.t * Ident.t * Desc.Type.t * source
    | Class_type of Origin.t * Ident.t * Desc.Class_type.t * source
    | Module_type of Origin.t * Ident.t * Desc.Module_type.t * source
    | Module of Origin.t * Ident.t * Desc.Module.t * source
    | Declare_module of Origin.t * Ident.t

end

module Graph : sig

  type t = graph

  val empty : t

  val add : t -> Component.t list -> t * Diff.t

  val merge : t -> Diff.t -> t

  val find_type : t -> Path.t -> Type.t

  val find_class_type : t -> Path.t -> Class_type.t

  val find_module_type : t -> Path.t -> Module_type.t

  val find_module : t -> Path.t -> Module.t

  val is_type_path_visible : t -> Path.t -> bool

  val is_class_type_path_visible : t -> Path.t -> bool

  val is_module_type_path_visible : t -> Path.t -> bool

  val is_module_path_visible : t -> Path.t -> bool

end
