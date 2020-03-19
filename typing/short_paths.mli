
module Desc = Short_paths_graph.Desc

(** A basis is a mutable set of consistent global (persistent) modules.

    In [Env], this list isn't represented explicitly.  Instead persistent
    modules are loaded on demand (and a consistency table is maintained along
    to ensure they are compatible with current environment).

    In short path representation, a basis exists outside of a specific
    environment.
    The short path environment is built by adding local definitions from an
    [Env.t] on top of a basis.
    When new global modules are loaded, the basis is invalidated and rebuilt on
    demand.

    Note: adding a new global module can reduce the short-paths of a prefix of
    an environment that was type checking without referencing this global
    module.
    It is somehow "non-monotonous" -- there is a subtle interaction between
    what shortest paths are and the set of global modules.
*)

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

val find_type_simple : t -> Path.t -> Path.t

type class_type_result = int list option * Path.t

val find_class_type : t -> Path.t -> class_type_result

val find_class_type_simple : t -> Path.t -> Path.t

val find_module_type : t -> Path.t -> Path.t

val find_module : t -> Path.t -> Path.t
