
val hidden : string -> bool

module String_map : Map.S with type key = string

module Age : Natural.S

module Dependency : Natural.S

module Height : Natural.S_no_zero

module Ident : sig

  type t

  val create : Dependency.t String_map.t -> Ident.t -> t

  val global : string -> Dependency.t -> t

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val hidden : t -> bool

  val height : t -> Height.t

  val ident : t -> Ident.t

  (* TODO remove when tracing goes *)
  val print : Format.formatter -> t -> unit

end

(* TODO remove when tracing goes *)
(* module Ident_map : Map.S with type key = Ident.t *)
(* module Ident_set : Set.S with type elt = Ident.t *)
module IdentId : Identifiable.S with type t := Ident.t
module Ident_map = IdentId.Map
module Ident_set = IdentId.Set

module Path : sig

  type t =
    | Pident of Ident.t
    | Pdot of
        { parent : t;
          name : string;
          hidden : bool; }
    | Papply of
        { func : t;
          arg : t; }

  val create : Dependency.t String_map.t -> Path.t -> t

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val hidden : t -> bool

  val height : t -> Height.t

  val path : t -> Path.t

  (* TODO remove when tracing goes *)
  val print : Format.formatter -> t -> unit

end

(* TODO remove when tracing goes *)
(* module Path_map : Map.S with type key = Path.t *)
(* module Path_set : Set.S with type elt = Path.t *)
module PathId : Identifiable.S with type t := Path.t
module Path_map = PathId.Map
module Path_set = PathId.Set

module Sort : sig

  type t =
    | Defined
    | Declared of Ident_set.t

end

module Origin : sig

  type t =
    | Dependency of Dependency.t
    | Dependencies of Dependency.t list
    | Environment of Age.t

  val equal : t -> t -> bool

  val hash : t -> int

  val pp : Format.formatter -> t -> unit
end

module Desc : sig

  module Type : sig

    type t =
      | Fresh
      | Nth of int
      | Subst of Path.t * int list
      | Alias of Path.t

  end

  module Module_type : sig

    type t =
      | Fresh
      | Alias of Path.t

  end

  module Module : sig

    type component =
      | Type of
          { name : string;
            hidden : bool;
            desc : Type.t; }
      | Module_type of
          { name : string;
            hidden : bool;
            desc : Module_type.t; }
      | Module of
          { name : string;
            hidden : bool;
            desc : t; }

    and components = component list

    and kind =
      | Signature of components Lazy.t
      | Functor of (Path.t -> t)

    and t =
      | Fresh of kind
      | Alias of Path.t

  end

  type t =
    | Type of Origin.t * Ident.t * Type.t * bool
    | Module_type of Origin.t * Ident.t * Module_type.t * bool
    | Module of Origin.t * Ident.t * Module.t * bool
    | Declare_type of Origin.t * Ident.t
    | Declare_module_type of Origin.t * Ident.t
    | Declare_module of Origin.t * Ident.t

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

  val types : graph -> t -> (Type.t * bool) String_map.t option

  val module_types : graph -> t -> (Module_type.t * bool) String_map.t option

  val modules : graph -> t -> (t * bool) String_map.t option

end

module Diff : sig

  module Item : sig

    type t =
      | Type of Ident.t * Type.t * Origin.t option
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

  val add : t -> Desc.t list -> t * Diff.t

  val merge : t -> Diff.t -> t

  val find_type : t -> Path.t -> Type.t

  val find_module_type : t -> Path.t -> Module_type.t

  val find_module : t -> Path.t -> Module.t

  val is_type_path_visible : t -> Path.t -> bool

  val is_module_type_path_visible : t -> Path.t -> bool

  val is_module_path_visible : t -> Path.t -> bool

end
