open Short_paths_loads

(* Generic definitions *)

module Ident = struct

  type t = Ident.t

  open Ident

  let equal t1 t2 =
    if t1.stamp = 0 then
      t2.stamp = 0
      && String.equal t1.name t2.name
    else t1.stamp = t2.stamp

  let compare t1 t2 =
    if t1.stamp = 0 then
      if t2.stamp = 0 then
        String.compare t1.name t2.name
      else -1
    else Pervasives.compare t1.stamp t2.stamp

  let name = Ident.name

  let global name =
    Ident.create_persistent name

end

module Ident_map = Map.Make(Ident)
module Ident_set = Set.Make(Ident)

module Path = struct

  type t = Path.t =
    | Pident of Ident.t
    | Pdot of t * string * int
    | Papply of t * t

  open Path

  let rec equal t1 t2 =
    match t1, t2 with
    | Pident id1, Pident id2 -> Ident.equal id1 id2
    | Pident _, Pdot _ -> false
    | Pident _, Papply _ -> false
    | Pdot _, Pident _ -> false
    | Pdot(parent1, name1, _), Pdot(parent2, name2, _) ->
        equal parent1 parent2
        && String.equal name1 name2
    | Pdot _, Papply _ -> false
    | Papply _, Pident _ -> false
    | Papply _, Pdot _ -> false
    | Papply(func1, arg1), Papply(func2, arg2) ->
        equal func1 func2
        && equal arg1 arg2

  let rec compare t1 t2 =
    match t1, t2 with
    | Pident id1, Pident id2 -> Ident.compare id1 id2
    | Pident _, Pdot _ -> -1
    | Pident _, Papply _ -> -1
    | Pdot _, Pident _ -> 1
    | Pdot(parent1, name1, _), Pdot(parent2, name2, _) ->
        let c = compare parent1 parent2 in
        if c <> 0 then c
        else String.compare name1 name2
    | Pdot _, Papply _ -> -1
    | Papply _, Pident _ -> 1
    | Papply _, Pdot _ -> 1
    | Papply(func1, arg1), Papply(func2, arg2) ->
        let c = compare func1 func2 in
        if c <> 0 then c
        else compare arg1 arg2

end

module Path_map = Map.Make(Path)
module Path_set = Set.Make(Path)

(* Subset of the type algebra that is relevant to short path *)

module Desc = struct

  module Type = struct

    type t =
      | Nth of int
      | Subst of Path.t * int list
      | Alias of Path.t
      | Fresh

  end

  module Class_type = struct

    type t =
      | Subst of Path.t * int list
      | Alias of Path.t
      | Fresh

  end

  module Module_type = struct

    type t =
      | Alias of Path.t
      | Fresh

  end

  module Module = struct

    type component =
      | Type of string * Type.t
      | Class_type of string * Class_type.t
      | Module_type of string * Module_type.t
      | Module of string * t

    and components = component list

    and kind =
      | Abstract
      | Signature of components Lazy.t
      | Functor of (Path.t -> t)

    and t =
      | Alias of Path.t
      | Fresh of kind

  end

  type source =
    | Local
    | Open

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

module rec Type : sig

  type t

  val base : Origin.t -> Ident.t -> Desc.Type.t -> t

  val child : Module.Normalized.t -> string -> Desc.Type.t -> t

  val unloaded : Module.Normalized.t -> string -> t

  val origin : Graph.t -> t -> Origin.t

  val path : Graph.t -> t -> Path.t

  type resolved =
    | Nth of int
    | Path of int list option * t

  val resolve : Graph.t -> t -> resolved

end = struct

  open Desc.Type

  type definition =
    | Alias of Path.t
    | Fresh
    | Nth of int
    | Subst of Path.t * int list
    | Unloaded

  type t =
    { origin : Origin.t;
      path : Path.t;
      definition : definition; }

  let definition_of_desc (desc : Desc.Type.t) =
    match desc with
    | Fresh -> Fresh
    | Nth n -> Nth n
    | Subst(p, ns) -> Subst(p, ns)
    | Alias alias -> Alias alias

  let base origin id desc =
    let path = Path.Pident id in
    let definition = definition_of_desc desc in
    { origin; path; definition }

  let child md name desc =
    let origin = Module.Normalized.origin md in
    let path = Path.Pdot(Module.Normalized.path md, name, 0) in
    let definition = definition_of_desc desc in
    { origin; path; definition }

  let unloaded md name =
    let origin = Module.Normalized.origin md in
    let path = Path.Pdot(Module.Normalized.path md, name, 0) in
    let definition = Unloaded in
    { origin; path; definition }

  let rec normalize_loop root t =
    match t.definition with
    | Fresh | Unloaded | Nth _ | Subst _ -> t
    | Alias alias -> begin
        match Graph.find_type root alias with
        | exception Not_found -> { t with definition = Fresh }
        | aliased -> normalize_loop root aliased
      end

  let normalize root t =
    let t =
      match t.definition with
      | Fresh | Nth _ | Subst _ | Alias _ -> t
      | Unloaded ->
          match Graph.find_type root t.path with
          | exception Not_found -> t
          | t -> t
    in
    normalize_loop root t

  let origin root t =
    (normalize root t).origin

  let path root t =
    (normalize root t).path

  type resolved =
    | Nth of int
    | Path of int list option * t

  let subst ns = function
    | Nth n -> Nth (List.nth ns n)
    | Path(None, p) -> Path(Some ns, p)
    | Path(Some ms, p) -> Path(Some (List.map (List.nth ns) ms), p)

  let rec resolve root t =
    match (normalize root t).definition with
    | Fresh | Unloaded -> Path(None, t)
    | Nth n -> Nth n
    | Subst(p, ns) -> begin
        match Graph.find_type root p with
        | exception Not_found -> Path(None, t)
        | aliased -> subst ns (resolve root aliased)
      end
    | Alias _ -> assert false

end

and Class_type : sig

  type t

  val base : Origin.t -> Ident.t -> Desc.Class_type.t -> t

  val child : Module.Normalized.t -> string -> Desc.Class_type.t -> t

  val unloaded : Module.Normalized.t -> string -> t

  val origin : Graph.t -> t -> Origin.t

  val path : Graph.t -> t -> Path.t

  type resolved = int list option * t

  val resolve : Graph.t -> t -> resolved

end = struct

  open Desc.Class_type

  type definition =
    | Alias of Path.t
    | Fresh
    | Subst of Path.t * int list
    | Unloaded

  type t =
    { origin : Origin.t;
      path : Path.t;
      definition : definition; }

  let definition_of_desc (desc : Desc.Class_type.t) =
    match desc with
    | Fresh -> Fresh
    | (Subst(p, ns)) -> Subst(p, ns)
    | (Alias alias) -> Alias alias

  let base origin id desc =
    let path = Path.Pident id in
    let definition = definition_of_desc desc in
    { origin; path; definition }

  let child md name desc =
    let origin = Module.Normalized.origin md in
    let path = Path.Pdot(Module.Normalized.path md, name, 0) in
    let definition = definition_of_desc desc in
    { origin; path; definition }

  let unloaded md name =
    let origin = Module.Normalized.origin md in
    let path = Path.Pdot(Module.Normalized.path md, name, 0) in
    let definition = Unloaded in
    { origin; path; definition }

  let rec normalize_loop root t =
    match t.definition with
    | Fresh | Unloaded | Subst _ -> t
    | Alias alias -> begin
        match Graph.find_class_type root alias with
        | exception Not_found -> { t with definition = Fresh }
        | aliased -> normalize_loop root aliased
      end

  let normalize root t =
    let t =
      match t.definition with
      | Fresh | Subst _ | Alias _ -> t
      | Unloaded ->
          match Graph.find_class_type root t.path with
          | exception Not_found -> t
          | t -> t
    in
    normalize_loop root t

  let origin root t =
    (normalize root t).origin

  let path root t =
    (normalize root t).path

  type resolved = int list option * t

  let subst ns = function
    | (None, p) -> (Some ns, p)
    | (Some ms, p) -> (Some (List.map (List.nth ns) ms), p)

  let rec resolve root t =
    match (normalize root t).definition with
    | Fresh | Unloaded -> (None, t)
    | Subst(p, ns) -> begin
        match Graph.find_class_type root p with
        | exception Not_found -> (None, t)
        | aliased -> subst ns (resolve root aliased)
      end
    | Alias _ -> assert false

end

and Module_type : sig

  type t

  val base : Origin.t -> Ident.t -> Desc.Module_type.t -> t

  val child : Module.Normalized.t -> string -> Desc.Module_type.t -> t

  val unloaded : Module.Normalized.t -> string -> t

  val origin : Graph.t -> t -> Origin.t

  val path : Graph.t -> t -> Path.t

end = struct

  open Desc.Module_type

  type definition =
    | Alias of Path.t
    | Fresh
    | Unloaded

  type t =
    { origin : Origin.t;
      path : Path.t;
      definition : definition; }

  let definition_of_desc (desc : Desc.Module_type.t) =
    match desc with
    | Fresh -> Fresh
    | Alias alias -> Alias alias

  let base origin id desc =
    let path = Path.Pident id in
    let definition = definition_of_desc desc in
    { origin; path; definition }

  let child md name desc =
    let origin = Module.Normalized.origin md in
    let path = Path.Pdot (Module.Normalized.path md, name, 0) in
    let definition = definition_of_desc desc in
    { origin; path; definition }

  let unloaded md name =
    let origin = Module.Normalized.origin md in
    let path = Path.Pdot (Module.Normalized.path md, name, 0) in
    let definition = Unloaded in
    { origin; path; definition }

  let rec normalize_loop root t =
    match t.definition with
    | Fresh | Unloaded -> t
    | Alias alias -> begin
        match Graph.find_module_type root alias with
        | exception Not_found -> { t with definition = Fresh }
        | aliased -> normalize_loop root aliased
      end

  let normalize root t =
    let t =
      match t.definition with
      | Fresh | Alias _ -> t
      | Unloaded ->
          match Graph.find_module_type root t.path with
          | exception Not_found -> t
          | t -> t
    in
    normalize_loop root t

  let origin root t =
    (normalize root t).origin

  let path root t =
    (normalize root t).path

end

and Module : sig

  type t

  module Normalized : sig

    type t

    val origin : t -> Origin.t

    val path : t -> Path.t

  end

  val base : Origin.t -> Ident.t -> Desc.Module.t -> t

  val unloaded_base : Dependency.t -> Ident.t -> t

  val child : Normalized.t -> string -> Desc.Module.t -> t

  val unloaded_child : Normalized.t -> string -> t

  val application : Normalized.t -> Normalized.t -> Desc.Module.t -> t

  val unloaded_application : Normalized.t -> Normalized.t -> t

  val origin : Graph.t -> t -> Origin.t

  val path : Graph.t -> t -> Path.t

  val types : Graph.t -> t -> Type.t String_map.t option

  val class_types : Graph.t -> t -> Class_type.t String_map.t option

  val module_types : Graph.t -> t -> Module_type.t String_map.t option

  val modules : Graph.t -> t -> Module.t String_map.t option

  val find_type : Graph.t -> t -> string -> Type.t

  val find_class_type : Graph.t -> t -> string -> Class_type.t

  val find_module_type : Graph.t -> t -> string -> Module_type.t

  val find_module : Graph.t -> t -> string -> Module.t

  val find_application : Graph.t -> t -> Path.t -> Module.t

  val normalize : Graph.t -> t -> Normalized.t

  val unnormalize : Normalized.t -> t

end = struct

  open Desc.Module

  type components =
    | Unforced of Desc.Module.components Lazy.t
    | Forced of
        { types : Type.t String_map.t;
          class_types : Class_type.t String_map.t;
          module_types : Module_type.t String_map.t;
          modules : t String_map.t; }

  and definition =
    | Alias of Path.t
    | Abstract
    | Signature of
        { mutable components : components }
    | Functor of
        { apply : Path.t -> Desc.Module.t;
          mutable applications : t Path_map.t; }
    | Unloaded

  and t =
    { origin : Origin.t;
      path : Path.t;
      definition : definition; }

  module Normalized = struct

    type nonrec t = t

    let origin t = t.origin

    let path t = t.path

  end

  let definition_of_desc (desc : Desc.Module.t) =
    match desc with
    | Fresh Abstract ->
        Abstract
    | Fresh (Signature components) ->
        let components = Unforced components in
        Signature { components }
    | Fresh (Functor apply) ->
        let applications = Path_map.empty in
        Functor { apply; applications }
    | Alias alias ->
        Alias alias

  let base origin id desc =
    let path = Path.Pident id in
    let definition = definition_of_desc desc in
    { origin; path; definition }

  let unloaded_base dep id =
    let origin = Origin.Dependency dep in
    let path = Path.Pident id in
    let definition = Unloaded in
    { origin; path; definition }

  let child md name desc =
    let origin = Module.Normalized.origin md in
    let path = Path.Pdot(Module.Normalized.path md, name, 0) in
    let definition = definition_of_desc desc in
    { origin; path; definition }

  let unloaded_child md name =
    let origin = Module.Normalized.origin md in
    let path = Path.Pdot(Module.Normalized.path md, name, 0) in
    let definition = Unloaded in
    { origin; path; definition }

  let application func arg desc =
    let func_origin = Module.Normalized.origin func in
    let arg_origin = Module.Normalized.origin arg in
    let origin = Origin.application func_origin arg_origin in
    let func_path = Module.Normalized.path func in
    let arg_path = Module.Normalized.path arg in
    let path = Path.Papply(func_path, arg_path) in
    let definition = definition_of_desc desc in
    { origin; path; definition }

  let unloaded_application func arg =
    let func_origin = Module.Normalized.origin func in
    let arg_origin = Module.Normalized.origin arg in
    let origin = Origin.application func_origin arg_origin in
    let func_path = Module.Normalized.path func in
    let arg_path = Module.Normalized.path arg in
    let path = Path.Papply(func_path, arg_path) in
    let definition = Unloaded in
    { origin; path; definition }

  let rec normalize_loop root t =
    match t.definition with
    | Abstract | Signature _ | Functor _ | Unloaded -> t
    | Alias alias -> begin
        match Graph.find_module root alias with
        | exception Not_found -> { t with definition = Abstract }
        | aliased -> normalize_loop root aliased
      end

  let normalize root t =
    let t =
      match t.definition with
      | Abstract | Signature _ | Functor _ | Alias _ -> t
      | Unloaded ->
          match Graph.find_module root t.path with
          | exception Not_found -> t
          | t -> t
    in
    normalize_loop root t

  let unnormalize t = t

  let origin root t =
    (normalize root t).origin

  let path root t =
    (normalize root t).path

  let definition t =
    (Module.unnormalize t).definition

  let force root t =
    let t = Module.normalize root t in
    match definition t with
    | Alias _ -> assert false
    | Abstract
    | Unloaded
    | Functor _
    | Signature { components = Forced _ } -> t
    | Signature ({ components = Unforced components; _} as r) -> begin
        let rec loop types class_types module_types modules = function
          | [] -> Forced { types; class_types; module_types; modules }
          | Type(name, desc) :: rest ->
              let typ = Type.child t name desc in
              let types = String_map.add name typ types in
              loop types class_types module_types modules rest
          | Class_type(name, desc) :: rest ->
              let clty = Class_type.child t name desc in
              let class_types = String_map.add name clty class_types in
              loop types class_types module_types modules rest
          | Module_type(name, desc) :: rest ->
              let mty = Module_type.child t name desc in
              let module_types = String_map.add name mty module_types in
              loop types class_types module_types modules rest
          | Module(name, desc) :: rest ->
              let md = Module.child t name desc in
              let modules = String_map.add name md modules in
              loop types class_types module_types modules rest
        in
        let empty = String_map.empty in
        let components = loop empty empty empty empty (Lazy.force components) in
        r.components <- components;
        t
      end

  let types root t =
    let t = force root t in
    match definition t with
    | Alias _ | Signature { components = Unforced _ } ->
        assert false
    | Unloaded | Abstract | Functor _ ->
        None
    | Signature { components = Forced { types; _ }; _ } ->
        Some types

  let class_types root t =
    let t = force root t in
    match definition t with
    | Alias _ | Signature { components = Unforced _ } ->
        assert false
    | Unloaded | Abstract | Functor _ ->
        None
    | Signature { components = Forced { class_types; _ } } ->
        Some class_types

  let module_types root t =
    let t = force root t in
    match definition t with
    | Alias _ | Signature { components = Unforced _ } ->
        assert false
    | Unloaded | Abstract | Functor _ ->
        None
    | Signature { components = Forced { module_types; _ } } ->
        Some module_types

  let modules root t =
    let t = force root t in
    match definition t with
    | Alias _ | Signature { components = Unforced _ } ->
        assert false
    | Unloaded | Abstract | Functor _ ->
        None
    | Signature { components = Forced { modules; _ } } ->
        Some modules

  let find_type root t name =
    let t = force root t in
    match definition t with
    | Alias _
    | Signature { components = Unforced _ } ->
        assert false
    | Abstract | Functor _ ->
        raise Not_found
    | Unloaded ->
        Type.unloaded t name
    | Signature { components = Forced { types; _ }; _ } ->
        String_map.find name types

  let find_class_type root t name =
    let t = force root t in
    match definition t with
    | Alias _
    | Signature { components = Unforced _ } ->
        assert false
    | Abstract | Functor _ ->
        raise Not_found
    | Unloaded ->
        Class_type.unloaded t name
    | Signature { components = Forced { class_types; _ }; _ } ->
        String_map.find name class_types

  let find_module_type root t name =
    let t = force root t in
    match definition t with
    | Alias _
    | Signature { components = Unforced _ } ->
        assert false
    | Abstract | Functor _ ->
        raise Not_found
    | Unloaded ->
        Module_type.unloaded t name
    | Signature { components = Forced { module_types; _ }; _ } ->
        String_map.find name module_types

  let find_module root t name =
    let t = force root t in
    match definition t with
    | Alias _
    | Signature { components = Unforced _ } ->
        assert false
    | Abstract | Functor _ ->
        raise Not_found
    | Unloaded ->
        Module.unloaded_child t name
    | Signature { components = Forced { modules; _ }; _ } ->
        String_map.find name modules

  let find_application root t path =
    let t = Module.normalize root t in
    match definition t with
    | Alias _ -> assert false
    | Abstract | Signature _ -> raise Not_found
    | Unloaded ->
        let arg = Graph.find_module root path in
        let arg = Module.normalize root arg in
        Module.unloaded_application t arg
    | Functor ({ apply; applications } as r)->
        let arg = Graph.find_module root path in
        let arg = Module.normalize root arg in
        let arg_path = Module.Normalized.path arg in
        match Path_map.find arg_path applications with
        | md -> md
        | exception Not_found ->
            let md = Module.application t arg (apply arg_path) in
            r.applications <- Path_map.add arg_path md applications;
            md

end

and Additions : sig

  module Item : sig

    type t =
      | Type of Ident.t * Type.t
      | Class_type of Ident.t * Class_type.t
      | Module_type of Ident.t * Module_type.t
      | Module of Ident.t * Module.t

    val origin : Graph.t -> t -> Origin.t

    val id : Graph.t -> t -> Ident.t

  end

  type t = Item.t list

end = struct

  module Item = struct

    type t =
      | Type of Ident.t * Type.t
      | Class_type of Ident.t * Class_type.t
      | Module_type of Ident.t * Module_type.t
      | Module of Ident.t * Module.t

    let origin root = function
      | Type(_, typ) -> Type.origin root typ
      | Class_type(_, clty) -> Class_type.origin root clty
      | Module_type(_, mty) -> Module_type.origin root mty
      | Module(_, md) -> Module.origin root md

    let id _root = function
      | Type(id, _) -> id
      | Class_type(id, _) -> id
      | Module_type(id, _) -> id
      | Module(id, _) -> id

  end

  type t = Item.t list

end

and Diff : sig

  module Item : sig

    type t = Ident.t * Module.t

  end

  type t = Item.t list

end = Diff

and Local_component : sig

  type source =
    | Definition
    | Open

  type t =
    | Type of Origin.t * Ident.t * Desc.Type.t * source
    | Class_type of Origin.t * Ident.t * Desc.Class_type.t * source
    | Module_type of Origin.t * Ident.t * Desc.Module_type.t * source
    | Module of Origin.t * Ident.t * Desc.Module.t * source

end = Local_component

and Persistent_component : sig

  type t =
    | Declare of Dependency.t * Ident.t
    | Load of Dependency.t * Ident.t * Desc.Module.t

end = Persistent_component

and Graph : sig

  type t

  val empty : t

  val create : t -> t

  val add_persistent :
    t -> Persistent_component.t list -> t * Additions.t * Diff.t

  val add : t -> Local_component.t list -> t * Additions.t

  val rebase : t -> Diff.t -> t

  val find_type : t -> Path.t -> Type.t

  val find_class_type : t -> Path.t -> Class_type.t

  val find_module_type : t -> Path.t -> Module_type.t

  val find_module : t -> Path.t -> Module.t

  val is_type_path_visible : t -> Path.t -> bool

  val is_class_type_path_visible : t -> Path.t -> bool

  val is_module_type_path_visible : t -> Path.t -> bool

  val is_module_path_visible : t -> Path.t -> bool

  val get_visible_type_path : t -> Path.t list -> Path.t option

  val get_visible_class_type_path : t -> Path.t list -> Path.t option

  val get_visible_module_type_path : t -> Path.t list -> Path.t option

  val get_visible_module_path : t -> Path.t list -> Path.t option

end = struct

  type defs =
    | Global of Ident.t
    | Local of Ident.t
    | Unambiguous of Ident.t
    | Ambiguous of Ident.t * Ident.t list

  type t =
    { types : Type.t Ident_map.t;
      class_types : Class_type.t Ident_map.t;
      module_types : Module_type.t Ident_map.t;
      modules : Module.t Ident_map.t;
      type_names : defs String_map.t;
      class_type_names : defs String_map.t;
      module_type_names : defs String_map.t;
      module_names : defs String_map.t; }

  let empty =
    { types = Ident_map.empty;
      class_types = Ident_map.empty;
      module_types = Ident_map.empty;
      modules = Ident_map.empty;
      type_names = String_map.empty;
      class_type_names = String_map.empty;
      module_type_names = String_map.empty;
      module_names = String_map.empty; }

  let create t = t

  let check_type_not_defined t id =
    match Ident_map.find id t.types with
    | exception Not_found -> ()
    | _ -> failwith "Graph.add: type already defined"

  let check_class_type_not_defined t id =
    match Ident_map.find id t.class_types with
    | exception Not_found -> ()
    | _ -> failwith "Graph.add: class type already defined"

  let check_module_type_not_defined t id =
    match Ident_map.find id t.module_types with
    | exception Not_found -> ()
    | _ -> failwith "Graph.add: module type already defined"

  let check_module_not_defined t id =
    match Ident_map.find id t.modules with
    | exception Not_found -> ()
    | _ -> failwith "Graph.add: module already defined"

  let add_name source id names =
    let name = Ident.name id in
    let defs =
      match source with
      | Local_component.Definition -> Local id
      | Local_component.Open -> begin
        match String_map.find name names with
        | exception Not_found -> Unambiguous id
        | Global _ -> Unambiguous id
        | Local id' -> Ambiguous(id, [id'])
        | Unambiguous id' -> Ambiguous(id, [id'])
        | Ambiguous(id', ids) -> Ambiguous(id, id' :: ids)
      end
    in
    String_map.add name defs names

  let add_global_name id names =
    let name = Ident.name id in
    let defs = Global id in
    String_map.add name defs names

  let merge_global_name id names =
    let name = Ident.name id in
    match String_map.find name names with
    | exception Not_found ->
        String_map.add name (Global id) names
    | _ -> names

  let add_persistent t descs =
    let rec loop_loads acc additions diff = function
      | [] -> acc, additions, diff
      | Persistent_component.Load(dep, id, desc) :: rest ->
          check_module_not_defined acc id;
          let origin = Origin.Dependency dep in
          let md = Module.base origin id desc in
          let modules = Ident_map.add id md acc.modules in
          let module_names = add_global_name id acc.module_names in
          let addition_item = Additions.Item.Module(id, md) in
          let additions = addition_item :: additions in
          let diff_item = id, md in
          let diff = diff_item :: diff in
          let acc = { acc with modules; module_names } in
          loop_loads acc additions diff rest
      | Persistent_component.Declare _ :: rest ->
          loop_loads acc additions diff rest
    in
    let rec loop_declarations acc diff = function
      | [] -> acc, diff
      | Persistent_component.Declare(dep, id) :: rest ->
          if Ident_map.mem id acc.modules then begin
            loop_declarations acc diff rest
          end else begin
            let md = Module.unloaded_base dep id in
            let modules = Ident_map.add id md acc.modules in
            let module_names = add_global_name id acc.module_names in
            let diff_item = id, md in
            let diff = diff_item :: diff in
            let acc = { acc with modules; module_names } in
            loop_declarations acc diff rest
          end
      | Persistent_component.Load _ :: rest ->
          loop_declarations acc diff rest
    in
    let t, additions, diff = loop_loads t [] [] descs in
    let t, diff = loop_declarations t diff descs in
    t, additions, diff

  let add t descs =
    let rec loop acc additions = function
      | [] -> acc, additions
      | Local_component.Type(origin, id, desc, source) :: rest ->
          check_type_not_defined acc id;
          let typ = Type.base origin id desc in
          let types = Ident_map.add id typ acc.types in
          let type_names = add_name source id acc.type_names in
          let addition = Additions.Item.Type(id, typ) in
          let additions = addition :: additions in
          let acc = { acc with types; type_names } in
          loop acc additions rest
      | Local_component.Class_type(origin,id, desc, source) :: rest ->
          check_class_type_not_defined acc id;
          let clty = Class_type.base origin id desc in
          let class_types = Ident_map.add id clty acc.class_types in
          let class_type_names = add_name source id acc.class_type_names in
          let addition = Additions.Item.Class_type(id, clty) in
          let additions = addition :: additions in
          let acc = { acc with class_types; class_type_names } in
          loop acc additions rest
      | Local_component.Module_type(origin,id, desc, source) :: rest ->
          check_module_type_not_defined acc id;
          let mty = Module_type.base origin id desc in
          let module_types = Ident_map.add id mty acc.module_types in
          let module_type_names = add_name source id acc.module_type_names in
          let addition = Additions.Item.Module_type(id, mty) in
          let additions = addition :: additions in
          let acc = { acc with module_types; module_type_names } in
          loop acc additions rest
      | Local_component.Module(origin, id, desc, source) :: rest ->
          check_module_not_defined acc id;
          let md = Module.base origin id desc in
          let modules = Ident_map.add id md acc.modules in
          let module_names = add_name source id acc.module_names in
          let addition = Additions.Item.Module(id, md) in
          let additions = addition :: additions in
          let acc = { acc with modules; module_names } in
          loop acc additions rest
    in
    loop t [] descs

  let rebase t diff =
    let rec loop acc = function
      | [] -> acc
      | (id, md) :: rest ->
        let modules = Ident_map.add id md acc.modules in
        let module_names = merge_global_name id acc.module_names in
        let acc = { acc with modules; module_names } in
        loop acc rest
    in
    loop t diff

  let rec find_module t path =
    match path with
    | Path.Pident id ->
        Ident_map.find id t.modules
    | Path.Pdot(p, name, _) ->
        let md = find_module t p in
        Module.find_module t md name
    | Path.Papply(p, arg) ->
        let md = find_module t p in
        Module.find_application t md arg

  let find_type t path =
    match path with
    | Path.Pident id ->
        Ident_map.find id t.types
    | Path.Pdot(p, name, _) ->
        let md = find_module t p in
        Module.find_type t md name
    | Path.Papply _ ->
        raise Not_found

  let find_class_type t path =
    match path with
    | Path.Pident id ->
        Ident_map.find id t.class_types
    | Path.Pdot(p, name, _) ->
        let md = find_module t p in
        Module.find_class_type t md name
    | Path.Papply _ ->
        raise Not_found

  let find_module_type t path =
    match path with
    | Path.Pident id ->
        Ident_map.find id t.module_types
    | Path.Pdot(p, name, _) ->
        let md = find_module t p in
        Module.find_module_type t md name
    | Path.Papply _ ->
        raise Not_found

  let canonical_type_path t id =
    match Ident_map.find id t.types with
    | exception Not_found -> Path.Pident id
    | md -> Type.path t md

  let canonical_class_type_path t id =
    match Ident_map.find id t.class_types with
    | exception Not_found -> Path.Pident id
    | md -> Class_type.path t md

  let canonical_module_type_path t id =
    match Ident_map.find id t.module_types with
    | exception Not_found -> Path.Pident id
    | md -> Module_type.path t md

  let canonical_module_path t id =
    match Ident_map.find id t.modules with
    | exception Not_found -> Path.Pident id
    | md -> Module.path t md

  let rec is_module_path_visible t = function
    | Path.Pident id -> begin
        let name = Ident.name id in
        match String_map.find name t.module_names with
        | exception Not_found -> false
        | Local id' -> Ident.equal id id'
        | Global id' -> Ident.equal id id'
        | Unambiguous id' -> Ident.equal id id'
        | Ambiguous(id', ids) ->
          if not (Ident.equal id id') then false
          else begin
            let paths = List.map (canonical_module_path t) ids in
            let path = canonical_module_path t id in
            List.for_all (Path.equal path) paths
          end
      end
    | Path.Pdot(path, _, _) ->
        is_module_path_visible t path
    | Path.Papply(path1, path2) ->
        is_module_path_visible t path1
        && is_module_path_visible t path2

  let is_type_path_visible t = function
    | Path.Pident id -> begin
        let name = Ident.name id in
        match String_map.find name t.type_names with
        | exception Not_found -> false
        | Local id' -> Ident.equal id id'
        | Global id' -> Ident.equal id id'
        | Unambiguous id' -> Ident.equal id id'
        | Ambiguous(id', ids) ->
          if not (Ident.equal id id') then false
          else begin
            let paths = List.map (canonical_type_path t) ids in
            let path = canonical_type_path t id in
            List.for_all (Path.equal path) paths
          end
      end
    | Path.Pdot(path, _, _) -> is_module_path_visible t path
    | Path.Papply _ ->
        failwith
          "Short_paths_graph.Graph.is_type_path_visible: \
           invalid type path"

  let is_class_type_path_visible t = function
    | Path.Pident id -> begin
        let name = Ident.name id in
        match String_map.find name t.class_type_names with
        | exception Not_found -> false
        | Local id' -> Ident.equal id id'
        | Global id' -> Ident.equal id id'
        | Unambiguous id' -> Ident.equal id id'
        | Ambiguous(id', ids) ->
          if not (Ident.equal id id') then false
          else begin
            let paths = List.map (canonical_class_type_path t) ids in
            let path = canonical_class_type_path t id in
            List.for_all (Path.equal path) paths
          end
      end
    | Path.Pdot(path, _, _) -> is_module_path_visible t path
    | Path.Papply _ ->
        failwith
          "Short_paths_graph.Graph.is_class_type_path_visible: \
           invalid class type path"

  let is_module_type_path_visible t = function
    | Path.Pident id -> begin
        let name = Ident.name id in
        match String_map.find name t.module_type_names with
        | exception Not_found -> false
        | Local id' -> Ident.equal id id'
        | Global id' -> Ident.equal id id'
        | Unambiguous id' -> Ident.equal id id'
        | Ambiguous(id', ids) ->
          if not (Ident.equal id id') then false
          else begin
            let paths = List.map (canonical_module_type_path t) ids in
            let path = canonical_module_type_path t id in
            List.for_all (Path.equal path) paths
          end
      end
    | Path.Pdot(path, _, _) -> is_module_path_visible t path
    | Path.Papply _ ->
        failwith
          "Short_paths_graph.Graph.is_module_type_path_visible: \
           invalid module type path"

    let rec get_visible_type_path graph = function
      | [] -> None
      | path :: rest ->
          let visible = is_type_path_visible graph path in
          if visible then Some path
          else get_visible_type_path graph rest

    let rec get_visible_class_type_path graph = function
      | [] -> None
      | path :: rest ->
          let visible = is_class_type_path_visible graph path in
          if visible then Some path
          else get_visible_class_type_path graph rest

    let rec get_visible_module_type_path graph = function
      | [] -> None
      | path :: rest ->
          let visible = is_module_type_path_visible graph path in
          if visible then Some path
          else get_visible_module_type_path graph rest

    let rec get_visible_module_path graph = function
      | [] -> None
      | path :: rest ->
          let visible = is_module_path_visible graph path in
          if visible then Some path
          else get_visible_module_path graph rest

end

module Basis = struct

  type t = Graph.t

  let empty = Graph.empty

  let add = Graph.add_persistent

end

type graph = Graph.t
