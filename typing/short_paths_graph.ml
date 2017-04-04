
module String_map = Map.Make(String)

module Age = Natural.Make()

module Dependency = Natural.Make()

module Height = Natural.Make_no_zero()

let hidden name =
  if name <> "" && name.[0] = '_' then true
  else
    try
      for i = 1 to String.length name - 2 do
        if name.[i] = '_' && name.[i + 1] = '_' then
          raise Exit
      done;
      false
    with Exit -> true

module Ident = struct

  type t =
    | Global of
        { dep : Dependency.t;
          name : string;
          hidden : bool; }
    | Local of
        { istamp : int;
          name : string;
          hidden : bool; }

  let global name dep =
    let hidden = hidden name in
    Global { dep; name; hidden }

  let create smap id =
    let istamp = id.Ident.stamp in
    if istamp = 0 then begin
      let name = id.Ident.name in
      let dep = String_map.find name smap in
      let hidden = hidden name in
      Global { dep; name; hidden }
    end else begin
      let name = id.Ident.name in
      let hidden =
        if !Clflags.unsafe_string && Ident.same id Predef.ident_bytes then true
        else hidden name
      in
      Local { istamp; name; hidden }
    end

  let compare t1 t2 =
    match t1, t2 with
    | Local _, Global _ -> -1
    | Global _, Local _ -> 1
    | Global { dep = dep1; _ }, Global { dep = dep2; _ } ->
        Dependency.compare dep1 dep2
    | Local { istamp = istamp1; _ }, Local { istamp = istamp2; _ } ->
        Pervasives.compare istamp1 istamp2

  let equal t1 t2 =
    match t1, t2 with
    | Local _, Global _ -> false
    | Global _, Local _ -> false
    | Global { dep = dep1; _ }, Global { dep = dep2; _ } ->
        Dependency.equal dep1 dep2
    | Local { istamp = istamp1; _ }, Local { istamp = istamp2; _ } ->
        istamp1 = istamp2

  let hidden = function
    | Global { hidden; _ } -> hidden
    | Local { hidden; _ } -> hidden

  let name = function
    | Global { name; _ } -> name
    | Local { name; _ } -> name

  let height t =
    if hidden t then Height.maximum
    else Height.one

  let ident = function
    | Global { name; _ } ->
        Ident.create_persistent name
    | Local { istamp; name; _ } ->
        { Ident.stamp = istamp; name; flags = 0 }

end

module Path = struct

  type t =
    | Pident of Ident.t
    | Pdot of
        { parent : t;
          name : string;
          hidden : bool; }
    | Papply of
        { func : t;
          arg : t; }

  let create smap p =
    let rec loop smap = function
      | Path.Pident id -> Pident (Ident.create smap id)
      | Path.Pdot(parent, name, _) ->
          let parent = loop smap parent in
          let hidden = hidden name in
          Pdot { parent; name; hidden }
      | Path.Papply(func, arg) ->
          let func = loop smap func in
          let arg = loop smap arg in
          Papply { func; arg }
    in
    loop smap p

  let compare t1 t2 =
    let rec loop t1 t2 =
      match t1, t2 with
      | Papply _, Pident _ -> -1
      | Pdot _, Pident _ -> -1
      | Papply _, Pdot _ -> -1
      | Pident _, Papply _ -> 1
      | Pident _, Pdot _ -> 1
      | Pdot _, Papply _ -> 1
      | Pident id1, Pident id2 -> Ident.compare id1 id2
      | Pdot { parent = parent1; name = name1; _ },
        Pdot { parent = parent2; name = name2; _ } ->
          let c = loop parent1 parent2 in
          if c <> 0 then c
          else String.compare name1 name2
      | Papply { func = func1; arg = arg1 },
        Papply { func = func2; arg = arg2} ->
          let c = loop func1 func2 in
          if c <> 0 then c
          else loop arg1 arg2
    in
    loop t1 t2

  let equal t1 t2 =
    let rec loop t1 t2 =
      match t1, t2 with
      | Papply _, Pident _ -> false
      | Pdot _, Pident _ -> false
      | Papply _, Pdot _ -> false
      | Pident _, Papply _ -> false
      | Pident _, Pdot _ -> false
      | Pdot _, Papply _ -> false
      | Pident id1, Pident id2 -> Ident.equal id1 id2
      | Pdot { parent = parent1; name = name1; _ },
        Pdot { parent = parent2; name = name2; _ } ->
          loop parent1 parent2
          && String.equal name1 name2
      | Papply { func = func1; arg = arg1 },
        Papply { func = func2; arg = arg2} ->
          loop func1 func2
          && loop arg1 arg2
    in
    loop t1 t2

  let hidden t =
    let rec loop = function
      | Pident id -> Ident.hidden id
      | Pdot { parent; hidden; _ } ->
          hidden || loop parent
      | Papply { func; arg; _ } ->
          loop func || loop arg
    in
    loop t

  let height t =
    let rec loop = function
      | Pident id ->
          Ident.height id
      | Pdot { parent; name; hidden } ->
          if hidden then Height.maximum
          else Height.succ (loop parent)
      | Papply { func; arg } ->
          Height.plus (loop func) (loop arg)
    in
    loop t

  let path t =
    let rec loop = function
      | Pident id -> Path.Pident (Ident.ident id)
      | Pdot { parent; name; _ } ->
          let parent = loop parent in
          Path.Pdot(parent, name, 0)
      | Papply { func; arg } ->
          let func = loop func in
          let arg = loop arg in
          Path.Papply(func, arg)
    in
    loop t

end

module Ident_map = Map.Make(Ident)
module Ident_set = Set.Make(Ident)

module Path_map = Map.Make(Path)
module Path_set = Set.Make(Path)

module Origin = struct

  type t =
    | Dependency of Dependency.t
    | Dependencies of Dependency.t list
    | Environment of Age.t

  let rec deps_add dep deps =
    match deps with
    | [] -> [dep]
    | dep' :: rest ->
      if Dependency.equal dep dep' then
        deps
      else if Dependency.less_than dep dep' then
        dep :: deps
      else
        dep' :: deps_add dep rest

  let rec deps_union deps1 deps2 =
    match deps1, deps2 with
    | [], _ -> deps2
    | _, [] -> deps1
    | dep1 :: rest1, dep2 :: rest2 ->
        if Dependency.equal dep1 dep2 then
          dep1 :: deps_union rest1 rest2
        else if Dependency.less_than dep1 dep2 then
          dep1 :: deps_union rest1 deps2
        else
          dep2 :: deps_union deps1 rest2

  let rec deps_equal deps1 deps2 =
    match deps1, deps2 with
    | [], [] -> true
    | [], _ :: _ -> false
    | _ :: _, [] -> false
    | dep1 :: rest1, dep2 :: rest2 ->
        Dependency.equal dep1 dep2
        && deps_equal rest1 rest2

  let application t1 t2 =
    match t1, t2 with
    | Dependency dep1, Dependency dep2 ->
        if Dependency.equal dep1 dep2 then t1
        else if Dependency.less_than dep1 dep2 then
          Dependencies [dep1; dep2]
        else
          Dependencies [dep2; dep1]
    | Dependency dep1, Dependencies deps2 ->
        Dependencies (deps_add dep1 deps2)
    | Dependency _, Environment _ -> t2
    | Dependencies deps1, Dependency dep2 ->
        Dependencies (deps_add dep2 deps1)
    | Dependencies deps1, Dependencies deps2 ->
        Dependencies (deps_union deps1 deps2)
    | Dependencies _, Environment _ -> t2
    | Environment _, Dependency _ -> t1
    | Environment _, Dependencies _ -> t1
    | Environment age1, Environment age2 ->
        Environment (Age.max age1 age2)

  let equal t1 t2 =
    match t1, t2 with
    | Dependency dep1, Dependency dep2 -> Dependency.equal dep1 dep2
    | Dependency _, Dependencies _ -> false
    | Dependency _, Environment _ -> false
    | Dependencies _, Dependency _ -> false
    | Dependencies deps1, Dependencies deps2 -> deps_equal deps1 deps2
    | Dependencies _, Environment _ -> false
    | Environment _, Dependency _ -> false
    | Environment _, Dependencies _ -> false
    | Environment env1, Environment env2 -> Age.equal env1 env2

  let hash = Hashtbl.hash

end

module Sort = struct

  type t =
    | Defined
    | Declared of Ident_set.t

  let application t1 t2 =
    match t1, t2 with
    | Defined, Defined -> Defined
    | Defined, Declared _ -> t2
    | Declared _, Defined -> t1
    | Declared ids1, Declared ids2 -> Declared (Ident_set.union ids1 ids2)

end

module Desc = struct

  module Type = struct

    type t =
      | Fresh
      | Nth of int
      | Subst of Path.t * int list
      | Alias of Path.t

  end

  module Module_type = struct

    type t =
      | Fresh
      | Alias of Path.t

  end

  module Module = struct

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

(* CR lwhite: Should probably short-circuit indirections if they are to
    canonical entries older than the indirection. *)
module rec Type : sig

  type t

  val base : Origin.t -> Ident.t -> Desc.Type.t option -> t

  val child : Graph.t -> Module.t -> string -> bool -> Desc.Type.t option -> t

  val declare : Origin.t -> Ident.t -> t

  val declaration : t -> Origin.t option

  val origin : Graph.t -> t -> Origin.t

  val path : Graph.t -> t -> Path.t

  val sort : Graph.t -> t -> Sort.t

  type resolved =
    | Nth of int
    | Path of int list option * t

  val resolve : Graph.t -> t -> resolved

end = struct

  open Desc.Type

  type definition =
    | Indirection of Path.t
    | Defined
    | Nth of int
    | Subst of Path.t * int list
    | Unknown

  type t =
    | Declaration of
        { origin : Origin.t;
          id : Ident.t; }
    | Definition of
        { origin : Origin.t;
          path : Path.t;
          sort : Sort.t;
          definition : definition; }

  let definition_of_desc (desc : Desc.Type.t option) =
    match desc with
    | None -> Unknown
    | Some Fresh -> Defined
    | Some (Nth n) -> Nth n
    | Some (Subst(p, ns)) -> Subst(p, ns)
    | Some (Alias alias) -> Indirection alias

  let base origin id desc =
    let path = Path.Pident id in
    let sort = Sort.Defined in
    let definition = definition_of_desc desc in
    Definition { origin; path; sort; definition }

  let child root md name hidden desc =
    let origin = Module.origin root md in
    let sort = Module.sort root md in
    let parent = Module.path root md in
    let path = Path.Pdot { parent; name; hidden } in
    let definition = definition_of_desc desc in
    Definition { origin; path; sort; definition }

  let declare origin id =
    Declaration { origin; id }

  let declaration t =
    match t with
    | Definition _ -> None
    | Declaration { origin; _} -> Some origin

  let raw_path t =
    match t with
    | Definition { path; _ } -> path
    | Declaration { id; _ } -> Path.Pident id

  let normalize root t =
    let rec loop root t =
      match t with
      | Declaration _ -> t
      | Definition { definition = Defined | Unknown | Nth _ | Subst _ } -> t
      | Definition ({ definition = Indirection alias } as r) -> begin
          match Graph.find_type root alias with
          | exception Not_found -> Definition { r with definition = Unknown }
          | aliased -> loop root aliased
        end
    in
    match t with
    | Definition { sort = Sort.Defined } -> loop root t
    | Definition { sort = Sort.Declared _ } | Declaration _ ->
        match Graph.find_type root (raw_path t) with
        | exception Not_found -> loop root t
        | t -> loop root t

  let origin root t =
    match normalize root t with
    | Declaration { origin; _ }
    | Definition { origin; _ } -> origin

  let path root t =
    match normalize root t with
    | Declaration { id; _ } -> Path.Pident id
    | Definition { path; _ } -> path

  let sort root t =
    match normalize root t with
    | Declaration { id; _ } -> Sort.Declared (Ident_set.singleton id)
    | Definition { sort; _ } -> sort

  type resolved =
    | Nth of int
    | Path of int list option * t

  let subst ns = function
    | Nth n -> Nth (List.nth ns n)
    | Path(None, p) -> Path(Some ns, p)
    | Path(Some ms, p) -> Path(Some (List.map (List.nth ns) ms), p)

  let resolve root t =
    let rec loop root t =
      match normalize root t with
      | Declaration _ -> Path(None, t)
      | Definition { definition = Defined | Unknown } -> Path(None, t)
      | Definition { definition = Nth n } -> Nth n
      | Definition { definition = Subst(p, ns) } -> begin
          match Graph.find_type root p with
          | exception Not_found -> Path(None, t)
          | aliased -> subst ns (loop root aliased)
        end
      | Definition { definition = Indirection _ } -> assert false
    in
    loop root t

end

and Module_type : sig

  type t

  val base : Origin.t -> Ident.t -> Desc.Module_type.t option -> t

  val child :
    Graph.t -> Module.t -> string -> bool -> Desc.Module_type.t option -> t

  val declare : Origin.t -> Ident.t -> t

  val declaration : t -> Origin.t option

  val origin : Graph.t -> t -> Origin.t

  val path : Graph.t -> t -> Path.t

  val sort : Graph.t -> t -> Sort.t

end = struct

  open Desc.Module_type

  type definition =
    | Indirection of Path.t
    | Defined
    | Unknown

  type t =
    | Declaration of
        { origin : Origin.t;
          id : Ident.t; }
    | Definition of
        { origin : Origin.t;
          path : Path.t;
          sort : Sort.t;
          definition : definition; }

  let base origin id desc =
    let path = Path.Pident id in
    let sort = Sort.Defined in
    let definition =
      match desc with
      | None -> Unknown
      | Some Fresh -> Defined
      | Some (Alias alias) -> Indirection alias
    in
    Definition { origin; path; sort; definition }

  let child root md name hidden desc =
    let origin = Module.origin root md in
    let sort = Module.sort root md in
    let parent = Module.path root md in
    let path = Path.Pdot { parent; name; hidden } in
    let definition =
      match desc with
      | None -> Unknown
      | Some Fresh -> Defined
      | Some (Alias alias) -> Indirection alias
    in
    Definition { origin; path; sort; definition }

  let declare origin id =
    Declaration { origin; id }

  let declaration t =
    match t with
    | Definition _ -> None
    | Declaration { origin; _} -> Some origin

  let raw_path t =
    match t with
    | Definition { path; _ } -> path
    | Declaration { id; _ } -> Path.Pident id

  let normalize root t =
    let rec loop root t =
      match t with
      | Declaration _ -> t
      | Definition { definition = Defined | Unknown } -> t
      | Definition ({ definition = Indirection alias } as r) -> begin
          match Graph.find_module_type root alias with
          | exception Not_found -> Definition { r with definition = Unknown }
          | aliased -> loop root aliased
        end
    in
    match t with
    | Definition { sort = Sort.Defined } -> loop root t
    | Definition { sort = Sort.Declared _ } | Declaration _ ->
        match Graph.find_module_type root (raw_path t) with
        | exception Not_found -> loop root t
        | t -> loop root t

  let origin root t =
    match normalize root t with
    | Declaration { origin; _ } | Definition { origin; _ } ->
        origin

  let path root t =
    match normalize root t with
    | Declaration { id; _ } -> Path.Pident id
    | Definition { path; _ } -> path

  let sort root t =
    match normalize root t with
    | Declaration { id; _ } -> Sort.Declared (Ident_set.singleton id)
    | Definition { sort; _ } -> sort

end

and Module : sig

  type t

  val base : Origin.t -> Ident.t -> Desc.Module.t option -> t

  val child : Graph.t -> t -> string -> bool -> Desc.Module.t option -> t

  val application : Graph.t -> t -> t -> Desc.Module.t option -> t

  val declare : Origin.t -> Ident.t -> t

  val declaration : t -> Origin.t option

  val origin : Graph.t -> t -> Origin.t

  val path : Graph.t -> t -> Path.t

  val sort : Graph.t -> t -> Sort.t

  val types : Graph.t -> t -> (Type.t * bool) String_map.t option

  val module_types : Graph.t -> t -> (Module_type.t * bool) String_map.t option

  val modules : Graph.t -> t -> (Module.t * bool) String_map.t option

  val find_type : Graph.t -> t -> string -> bool -> Type.t

  val find_module_type : Graph.t -> t -> string -> bool -> Module_type.t

  val find_module : Graph.t -> t -> string -> bool -> Module.t

  val find_application : Graph.t -> t -> Path.t -> Module.t

end = struct

  open Desc.Module

  type components =
    | Unforced of Desc.Module.components Lazy.t
    | Forced of
        { types : (Type.t * bool)String_map.t;
          module_types : (Module_type.t * bool)String_map.t;
          modules : (t * bool) String_map.t; }

  and definition =
    | Indirection of Path.t
    | Signature of
        { mutable components : components }
    | Functor of
        { apply : Path.t -> Desc.Module.t;
          mutable applications : t Path_map.t; }
    | Unknown

  and t =
    | Declaration of
        { origin : Origin.t;
          id : Ident.t; }
    | Definition of
        { origin : Origin.t;
          path : Path.t;
          sort : Sort.t;
          definition : definition; }

  let base origin id desc =
    let path = Path.Pident id in
    let sort = Sort.Defined in
    let definition =
      match desc with
      | None -> Unknown
      | Some (Fresh (Signature components)) ->
          let components = Unforced components in
          Signature { components }
      | Some (Fresh (Functor apply)) ->
          let applications = Path_map.empty in
          Functor { apply; applications }
      | Some (Alias alias) ->
          Indirection alias
    in
    Definition { origin; path; sort; definition }

  let child root md name hidden desc =
    let origin = Module.origin root md in
    let sort = Module.sort root md in
    let parent = Module.path root md in
    let path = Path.Pdot { parent; name; hidden } in
    let definition =
      match desc with
      | None -> Unknown
      | Some (Fresh (Signature components)) ->
          let components = Unforced components in
          Signature { components }
      | Some (Fresh (Functor apply)) ->
          let applications = Path_map.empty in
          Functor { apply; applications }
      | Some (Alias alias) ->
          Indirection alias
    in
    Definition { origin; path; sort; definition }

  let application root func arg desc =
    let func_origin = Module.origin root func in
    let arg_origin = Module.origin root arg in
    let origin = Origin.application func_origin arg_origin in
    let func_sort = Module.sort root func in
    let arg_sort = Module.sort root arg in
    let sort = Sort.application func_sort arg_sort in
    let func = Module.path root func in
    let arg = Module.path root arg in
    let path = Path.Papply { func; arg; } in
    let definition =
      match desc with
      | None -> Unknown
      | Some (Fresh (Signature components)) ->
          let components = Unforced components in
          Signature { components }
      | Some (Fresh (Functor apply)) ->
          let applications = Path_map.empty in
          Functor { apply; applications }
      | Some (Alias alias) ->
          Indirection alias
    in
    Definition { origin; path; sort; definition }

  let declare origin id =
    Declaration { origin; id }

  let declaration t =
    match t with
    | Definition _ -> None
    | Declaration { origin; _} -> Some origin

  let raw_path t =
    match t with
    | Definition { path; _ } -> path
    | Declaration { id; _ } -> Path.Pident id

  let normalize root t =
    let rec loop root t =
      match t with
      | Declaration _ -> t
      | Definition { definition = Signature _ | Functor _ | Unknown } -> t
      | Definition ({ definition = Indirection alias } as r) -> begin
          match Graph.find_module root alias with
          | exception Not_found -> Definition { r with definition = Unknown }
          | aliased -> loop root aliased
        end
    in
    match t with
    | Definition { sort = Sort.Defined } -> loop root t
    | Definition { sort = Sort.Declared _ } | Declaration _ ->
        match Graph.find_module root (raw_path t) with
        | exception Not_found -> loop root t
        | t -> loop root t

  let origin root t =
    match normalize root t with
    | Declaration { origin; _ } | Definition { origin; _ } ->
        origin

  let path root t =
    match normalize root t with
    | Declaration { id; _ } -> Path.Pident id
    | Definition { path; _ } -> path

  let sort root t =
    match normalize root t with
    | Declaration { id; _ } -> Sort.Declared (Ident_set.singleton id)
    | Definition { sort; _ } -> sort

  let definition t =
    match t with
    | Declaration _ -> Unknown
    | Definition { definition; _ } -> definition

  let force root t =
    let t = normalize root t in
    match definition t with
    | Indirection _ -> assert false
    | Unknown
    | Functor _
    | Signature { components = Forced _ } -> t
    | Signature ({ components = Unforced components; _} as r) -> begin
        let rec loop types module_types modules = function
          | [] -> Forced { types; module_types; modules }
          | Type { name; hidden; desc } :: rest ->
              let typ = Type.child root t name hidden (Some desc) in
              let types = String_map.add name (typ, hidden) types in
              loop types module_types modules rest
          | Module_type { name; hidden; desc } :: rest ->
              let mty = Module_type.child root t name hidden (Some desc) in
              let module_types = String_map.add name (mty, hidden) module_types in
              loop types module_types modules rest
          | Module { name; hidden; desc } :: rest ->
              let md = Module.child root t name hidden (Some desc) in
              let modules = String_map.add name (md, hidden) modules in
              loop types module_types modules rest
        in
        let empty = String_map.empty in
        let components = loop empty empty empty (Lazy.force components) in
        r.components <- components;
        t
      end

  let types root t =
    let t = force root t in
    match definition t with
    | Indirection _ | Signature { components = Unforced _ } ->
        assert false
    | Unknown | Functor _ ->
        None
    | Signature { components = Forced { types; _ }; _ } ->
        Some types

  let module_types root t =
    let t = force root t in
    match definition t with
    | Indirection _ | Signature { components = Unforced _ } ->
        assert false
    | Unknown | Functor _ ->
        None
    | Signature { components = Forced { module_types; _ } } ->
        Some module_types

  let modules root t =
    let t = force root t in
    match definition t with
    | Indirection _ | Signature { components = Unforced _ } ->
        assert false
    | Unknown | Functor _ ->
        None
    | Signature { components = Forced { modules; _ } } ->
        Some modules

  let find_type root t name hidden =
    let t = force root t in
    match definition t with
    | Indirection _
    | Signature { components = Unforced _ } ->
        assert false
    | Unknown ->
        Type.child root t name hidden None
    | Functor _ ->
        raise Not_found
    | Signature { components = Forced { types; _ }; _ } ->
        let typ, _ = String_map.find name types in
        typ

  let find_module_type root t name hidden =
    let t = force root t in
    match definition t with
    | Indirection _
    | Signature { components = Unforced _ } ->
        assert false
    | Unknown ->
        Module_type.child root t name hidden None
    | Functor _ ->
        raise Not_found
    | Signature { components = Forced { module_types; _ }; _ } ->
        let mty, _ = String_map.find name module_types in
        mty

  let find_module root t name hidden =
    let t = force root t in
    match definition t with
    | Indirection _
    | Signature { components = Unforced _ } ->
        assert false
    | Unknown ->
        Module.child root t name hidden None
    | Functor _ ->
        raise Not_found
    | Signature { components = Forced { modules; _ }; _ } ->
        let md, _ = String_map.find name modules in
        md

  let find_application root t path =
    let t = normalize root t in
    match definition t with
    | Indirection _ -> assert false
    | Signature _ -> raise Not_found
    | Unknown ->
        let arg = Graph.find_module root path in
        Module.application root t arg None
    | Functor ({ apply; applications } as r)->
        let arg = Graph.find_module root path in
        let arg_path = Module.path root arg in
        match Path_map.find arg_path applications with
        | md -> md
        | exception Not_found ->
            let md = Module.application root t arg (Some (apply arg_path)) in
            r.applications <- Path_map.add arg_path md applications;
            md

end

and Diff : sig

  module Item : sig

    type t =
      | Type of Ident.t * Type.t * Origin.t option
      | Module_type of Ident.t * Module_type.t * Origin.t option
      | Module of Ident.t * Module.t * Origin.t option

    val origin : Graph.t -> t -> Origin.t

    val id : Graph.t -> t -> Ident.t

    val previous : Graph.t -> t -> Origin.t option

  end

  type t = Item.t list

end = struct

  module Item = struct

    type t =
      | Type of Ident.t * Type.t * Origin.t option
      | Module_type of Ident.t * Module_type.t * Origin.t option
      | Module of Ident.t * Module.t * Origin.t option

    let origin root = function
      | Type(_, typ, _) -> Type.origin root typ
      | Module_type(_, mty, _) -> Module_type.origin root mty
      | Module(_, md, _) -> Module.origin root md

    let id _root = function
      | Type(id, _, _) -> id
      | Module_type(id, _, _) -> id
      | Module(id, _, _) -> id

    let previous _root = function
      | Type(_, _, prev) -> prev
      | Module_type(_, _, prev) -> prev
      | Module(_, _, prev) -> prev

  end

  type t = Item.t list

end

and Graph : sig

  type t

  val empty : t

  val add : t -> Desc.t list -> t * Diff.t

  val merge : t -> Diff.t -> t

  val find_type : t -> Path.t -> Type.t

  val find_module_type : t -> Path.t -> Module_type.t

  val find_module : t -> Path.t -> Module.t

  val is_type_path_visible : t -> Path.t -> bool

  val is_module_type_path_visible : t -> Path.t -> bool

  val is_module_path_visible : t -> Path.t -> bool

end = struct

  type defs =
    | Concrete of Ident.t
    | Unambiguous of Ident.t
    | Ambiguous of Ident.t * bool * Ident.t list

  type t =
    { types : Type.t Ident_map.t;
      module_types : Module_type.t Ident_map.t;
      modules : Module.t Ident_map.t;
      type_names : defs String_map.t;
      module_type_names : defs String_map.t;
      module_names : defs String_map.t; }

  let empty =
    { types = Ident_map.empty;
      module_types = Ident_map.empty;
      modules = Ident_map.empty;
      type_names = String_map.empty;
      module_type_names = String_map.empty;
      module_names = String_map.empty; }

  let previous_type t concrete id =
    match Ident_map.find id t.types with
    | exception Not_found -> None
    | prev ->
      if not concrete then failwith "Graph.add: type already defined";
      match Type.declaration prev with
      | None -> failwith "Graph.add: type already defined"
      | Some _ as o -> o

  let previous_module_type t concrete id =
    match Ident_map.find id t.module_types with
    | exception Not_found -> None
    | prev ->
      if not concrete then failwith "Graph.add: module type already defined";
      match Module_type.declaration prev with
      | None -> failwith "Graph.add: module type already defined"
      | Some _ as o -> o

  let previous_module t concrete id =
    match Ident_map.find id t.modules with
    | exception Not_found -> None
    | prev ->
      if not concrete then failwith "Graph.add: module already defined";
      match Module.declaration prev with
      | None -> failwith "Graph.add: module already defined"
      | Some _ as o -> o

  let add_name concrete id names =
    let name = Ident.name id in
    let defs =
      if concrete then begin
        (Concrete id)
      end else begin
        match String_map.find name names with
        | exception Not_found -> Unambiguous id
        | Concrete id' -> Ambiguous(id, true, [id'])
        | Unambiguous id' -> Ambiguous(id, false, [id'])
        | Ambiguous(id', conc, ids) -> Ambiguous(id, conc, id' :: ids)
      end
    in
    String_map.add name defs names

  let merge_name id names =
    let name = Ident.name id in
    match String_map.find name names with
    | exception Not_found ->
        String_map.add name (Concrete id) names
    | Concrete _ | Ambiguous(_, true, _) -> names
    | Ambiguous(id', false, ids) ->
        String_map.add name (Ambiguous(id', true, ids @ [id])) names
    | Unambiguous id' ->
        String_map.add name (Ambiguous(id', true, [id])) names

  let add t descs =
    let rec loop acc diff declarations = function
      | [] -> loop_declarations acc diff declarations
      | Desc.Type(origin, id, desc, concrete) :: rest ->
          let prev = previous_type acc concrete id in
          let typ = Type.base origin id (Some desc) in
          let types = Ident_map.add id typ acc.types in
          let type_names = add_name concrete id acc.type_names in
          let item = Diff.Item.Type(id, typ, prev) in
          let diff = item :: diff in
          let acc = { acc with types; type_names } in
          loop acc diff declarations rest
      | Desc.Module_type(origin,id, desc, concrete) :: rest ->
          let prev = previous_module_type acc concrete id in
          let mty = Module_type.base origin id (Some desc) in
          let module_types = Ident_map.add id mty acc.module_types in
          let module_type_names = add_name concrete id acc.module_type_names in
          let item = Diff.Item.Module_type(id, mty, prev) in
          let diff = item :: diff in
          let acc = { acc with module_types; module_type_names } in
          loop acc diff declarations rest
      | Desc.Module(origin,id, desc, concrete) :: rest ->
          let prev = previous_module acc concrete id in
          let md = Module.base origin id (Some desc) in
          let modules = Ident_map.add id md acc.modules in
          let module_names = add_name concrete id acc.module_names in
          let item = Diff.Item.Module(id, md, prev) in
          let diff = item :: diff in
          let acc = { acc with modules; module_names } in
          loop acc diff declarations rest
      | Desc.Declare_type(_, id) as decl :: rest ->
          let declarations = decl :: declarations in
          let type_names = add_name true id acc.type_names in
          let acc = { acc with type_names } in
          loop acc diff declarations rest
      | Desc.Declare_module_type(_, id) as decl :: rest ->
          let declarations = decl :: declarations in
          let module_type_names = add_name true id acc.module_type_names in
          let acc = { acc with module_type_names } in
          loop acc diff declarations rest
      | Desc.Declare_module(_, id) as decl :: rest ->
          let declarations = decl :: declarations in
          let module_names = add_name true id acc.module_names in
          let acc = { acc with module_names } in
          loop acc diff declarations rest
    and loop_declarations acc diff = function
      | [] -> acc, diff
      | Desc.Declare_type(origin, id) :: rest ->
          if Ident_map.mem id acc.types then begin
            loop_declarations acc diff rest
          end else begin
            let typ = Type.declare origin id in
            let types = Ident_map.add id typ acc.types in
            let acc = { acc with types } in
            loop_declarations acc diff rest
          end
      | Desc.Declare_module_type(origin, id) :: rest ->
          if Ident_map.mem id acc.module_types then begin
            loop_declarations acc diff rest
          end else begin
            let mty = Module_type.declare origin id in
            let module_types = Ident_map.add id mty acc.module_types in
            let acc = { acc with module_types } in
            loop_declarations acc diff rest
          end
      | Desc.Declare_module(origin, id) :: rest ->
          if Ident_map.mem id acc.modules then begin
            loop_declarations acc diff rest
          end else begin
            let md = Module.declare origin id in
            let modules = Ident_map.add id md acc.modules in
            let acc = { acc with modules } in
            loop_declarations acc diff rest
          end
      | ( Desc.Type _
        | Desc.Module_type _
        | Desc.Module _) :: _ -> assert false
    in
    loop t [] [] descs

  let merge t diff =
    let rec loop acc = function
      | [] -> acc
      | Diff.Item.Type(id, typ, _) :: rest ->
          let types = Ident_map.add id typ acc.types in
          let type_names = merge_name id acc.type_names in
          let acc = { acc with types; type_names } in
          loop acc rest
      | Diff.Item.Module_type(id, mty, _) :: rest ->
          let module_types = Ident_map.add id mty acc.module_types in
          let module_type_names = merge_name id acc.module_type_names in
          let acc = { acc with module_types; module_type_names } in
          loop acc rest
      | Diff.Item.Module(id, md, _) :: rest ->
          let modules = Ident_map.add id md acc.modules in
          let module_names = merge_name id acc.module_names in
          let acc = { acc with modules; module_names } in
          loop acc rest
    in
    loop t diff

  let rec find_module t path =
    match path with
    | Path.Pident id ->
        Ident_map.find id t.modules
    | Path.Pdot { parent; name; hidden } ->
        let md = find_module t parent in
        Module.find_module t md name hidden
    | Path.Papply { func; arg } ->
        let md = find_module t func in
        Module.find_application t md arg

  let find_type t path =
    match path with
    | Path.Pident id ->
        Ident_map.find id t.types
    | Path.Pdot { parent; name; hidden } ->
        let md = find_module t parent in
        Module.find_type t md name hidden
    | Path.Papply _ ->
        raise Not_found

  let find_module_type t path =
    match path with
    | Path.Pident id ->
        Ident_map.find id t.module_types
    | Path.Pdot { parent; name; hidden } ->
        let md = find_module t parent in
        Module.find_module_type t md name hidden
    | Path.Papply _ ->
        raise Not_found

  let canonical_type_path t id =
    match Ident_map.find id t.types with
    | exception Not_found -> Path.Pident id
    | md -> Type.path t md

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
        | Concrete id' -> Ident.equal id id'
        | Unambiguous id' -> Ident.equal id id'
        | Ambiguous(id', _, ids) ->
          if not (Ident.equal id id') then false
          else begin
            let paths = List.map (canonical_module_path t) ids in
            let path = canonical_module_path t id in
            List.for_all (Path.equal path) paths
          end
      end
    | Path.Pdot { parent; _ } ->
        is_module_path_visible t parent
    | Path.Papply { func; arg } ->
        is_module_path_visible t func
        && is_module_path_visible t arg

  let is_type_path_visible t = function
    | Path.Pident id -> begin
        let name = Ident.name id in
        match String_map.find name t.type_names with
        | exception Not_found -> false
        | Concrete id' -> Ident.equal id id'
        | Unambiguous id' -> Ident.equal id id'
        | Ambiguous(id', _, ids) ->
          if not (Ident.equal id id') then false
          else begin
            let paths = List.map (canonical_type_path t) ids in
            let path = canonical_type_path t id in
            List.for_all (Path.equal path) paths
          end
      end
    | Path.Pdot { parent; _ } -> is_module_path_visible t parent
    | Path.Papply _ ->
        failwith
          "Short_paths_graph.Graph.is_type_path_visible: \
           invalid type path"

  let is_module_type_path_visible t = function
    | Path.Pident id -> begin
        let name = Ident.name id in
        match String_map.find name t.module_type_names with
        | exception Not_found -> false
        | Concrete id' -> Ident.equal id id'
        | Unambiguous id' -> Ident.equal id id'
        | Ambiguous(id', _, ids) ->
          if not (Ident.equal id id') then false
          else begin
            let paths = List.map (canonical_module_type_path t) ids in
            let path = canonical_module_type_path t id in
            List.for_all (Path.equal path) paths
          end
      end
    | Path.Pdot { parent; _ } -> is_module_path_visible t parent
    | Path.Papply _ ->
        failwith
          "Short_paths_graph.Graph.is_module_type_path_visible: \
           invalid module type path"

end

type graph = Graph.t
