
module Desc = Short_paths_graph.Desc

module Basis : sig

  type t

  val create : unit -> t

  val add : t -> string -> unit

  val load : t -> string -> string list -> Desc.Module.t -> unit

end

type t

val initial : Basis.t -> t

val add : t -> Desc.t list Lazy.t -> t

val find_type : t -> Path.t -> Path.t

val find_module_type : t -> Path.t -> Path.t

val find_module : t -> Path.t -> Path.t
