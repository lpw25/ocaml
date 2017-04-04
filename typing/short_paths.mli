
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
      | Type of string * Type.t
      | Module_type of string * Module_type.t
      | Module of string * t

    and components = component list

    and kind =
      | Signature of components Lazy.t
      | Functor of (Path.t -> t)

    and t =
      | Fresh of kind
      | Alias of Path.t

  end

  type t =
    | Type of Ident.t * Type.t * bool
    | Module_type of Ident.t * Module_type.t * bool
    | Module of Ident.t * Module.t * bool
    | Declare_type of Ident.t
    | Declare_module_type of Ident.t
    | Declare_module of Ident.t

end

module Basis : sig

  type t

  val create : unit -> t

  val add : t -> string -> unit

  val load : t -> string -> string list -> string list -> Desc.Module.t -> unit

end

type t

val initial : Basis.t -> t

val add : t -> Desc.t list Lazy.t -> t

type type_result =
  | Nth of int
  | Path of int list option * Path.t

val find_type : t -> Path.t -> type_result

type type_resolution =
  | Nth of int
  | Subst of int list
  | Id

val find_type_resolution : t -> Path.t -> type_resolution

val find_module_type : t -> Path.t -> Path.t

val find_module : t -> Path.t -> Path.t
