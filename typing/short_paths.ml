
let tracing = false

let trace fmt =
  if tracing then Format.fprintf Format.err_formatter fmt
  else Format.ifprintf Format.err_formatter fmt

open Short_paths_graph

module Desc = Desc

module Rev_deps = struct

  type t = Dependency.Set.t Dependency.Array.t

  let get = Dependency.Array.get

  let add t source target =
    let old_set = Dependency.Array.get t source in
    let new_set = Dependency.Set.add target old_set in
    Dependency.Array.set t source new_set

  let empty = Dependency.Array.empty

  let extend_up_to t next =
    match Dependency.pred next with
    | None -> t
    | Some curr ->
      if not (Dependency.Array.contains t curr) then
        Dependency.Array.extend t curr (fun _ -> Dependency.Set.empty)
      else t

  let before t origin1 origin2 =
    let open Origin in
    match origin1, origin2 with
    | Environment age1, Environment age2 -> Age.less_than age1 age2
    | Environment _, Dependency _ -> false
    | Environment _, Dependencies _ -> false
    | Dependency _, Environment _ -> true
    | Dependency dep1, Dependency dep2 ->
        let rev_dep = get t dep1 in
        Dependency.Set.mem dep2 rev_dep
    | Dependency dep1, Dependencies deps2 ->
        let rev_dep = get t dep1 in
        List.exists
          (fun dep2 -> Dependency.Set.mem dep2 rev_dep)
          deps2
    | Dependencies _, Environment _ -> true
    | Dependencies deps1, Dependency dep2 ->
        List.for_all
          (fun dep1 -> Dependency.Set.mem dep2 (get t dep1))
          deps1
    | Dependencies deps1, Dependencies deps2 ->
        let rev_dep =
          match deps1 with
          | [] -> failwith "Rev_deps.before: invalid origin"
          | dep1 :: deps1 ->
              List.fold_left
                (fun acc dep1 -> Dependency.Set.inter acc (get t dep1))
                (get t dep1) deps1
        in
        List.exists
          (fun dep2 -> Dependency.Set.mem dep2 rev_dep)
          deps2

end

module Origin_range_tbl = struct

  type 'a t =
    { mutable envs : 'a list Age.Map.t;
      mutable dep_keys : Dependency.Set.t;
      deps : 'a list Dependency.Tbl.t; }

  let create () =
    { envs = Age.Map.empty;
      dep_keys = Dependency.Set.empty;
      deps = Dependency.Tbl.create 0; }

  let add_dependency dep data t =
    t.dep_keys <- Dependency.Set.add dep t.dep_keys;
    let prev =
      match Dependency.Tbl.find t.deps dep with
      | exception Not_found -> []
      | prev -> prev
    in
    Dependency.Tbl.replace t.deps dep (data :: prev)

  let add_age age data t =
    let prev =
      match Age.Map.find age t.envs with
      | exception Not_found -> []
      | prev -> prev
    in
    t.envs <- Age.Map.add age (data :: prev) t.envs

  let add rev_deps origin data t =
    match origin with
    | Origin.Dependency dep -> add_dependency dep data t
    | Origin.Environment age -> add_age age data t
    | Origin.Dependencies deps -> begin
        let rev_dep_opt =
          List.fold_left
            (fun acc dep ->
               let rev_dep = Rev_deps.get rev_deps dep in
               match acc with
               | None -> Some rev_dep
               | Some acc -> Some (Dependency.Set.inter acc rev_dep))
            None deps
        in
        let rev_dep =
          match rev_dep_opt with
          | None -> failwith "Origin_range_tbl.add: invalid origin"
          | Some rev_dep -> rev_dep
        in
        match
          List.find
            (fun dep -> Dependency.Set.mem dep rev_dep)
            deps
        with
        | dep -> add_dependency dep data t
        | exception Not_found ->
          match Dependency.Set.choose rev_dep with
          | dep -> add_dependency dep data t
          | exception Not_found -> add_age Age.zero data t
      end

  let pop_dependency rev_dep t =
    let matching = Dependency.Set.inter rev_dep t.dep_keys in
    t.dep_keys <- Dependency.Set.diff t.dep_keys matching;
    let items =
      Dependency.Set.fold
        (fun dep acc ->
           let data = Dependency.Tbl.find t.deps dep in
           Dependency.Tbl.remove t.deps dep;
           trace "Popped %i todo items from Dep(%a)\n%!"
             (List.length data) Dependency.pp dep;
           List.rev_append data acc)
        matching
        []
    in
    let items =
      Age.Map.fold
        (fun age data acc ->
           trace "Popped %i todo items from Env(%a)\n%!"
             (List.length data) Age.pp age;
           List.rev_append data acc)
        t.envs items
    in
    t.envs <- Age.Map.empty;
    items

  let pop_age age t =
    let envs, first, matching = Age.Map.split age t.envs in
    let items =
      match first with
      | None -> []
      | Some first -> first
    in
    let items =
      Age.Map.fold
        (fun _ data acc -> List.rev_append data acc)
        matching items
    in
    t.envs <- envs;
    items

  let pop rev_deps origin t =
    match origin with
    | Origin.Dependency dep ->
        let rev_dep = Rev_deps.get rev_deps dep in
        pop_dependency rev_dep t
    | Origin.Dependencies deps ->
        let rev_dep_opt =
          List.fold_left
            (fun acc dep ->
               let rev_dep = Rev_deps.get rev_deps dep in
               match acc with
               | None -> Some rev_dep
               | Some acc -> Some (Dependency.Set.inter acc rev_dep))
            None deps
        in
        let rev_dep =
          match rev_dep_opt with
          | None -> failwith "Origin_range_tbl.pop: invalid origin"
          | Some rev_dep -> rev_dep
        in
        pop_dependency rev_dep t
    | Origin.Environment age ->
        pop_age age t

  let is_origin_empty rev_deps origin t =
    match origin with
    | Origin.Dependency dep ->
        if Age.Map.is_empty t.envs then false
        else begin
          let rev_dep = Rev_deps.get rev_deps dep in
          let matching = Dependency.Set.inter rev_dep t.dep_keys in
          Dependency.Set.is_empty matching
        end
    | Origin.Dependencies deps ->
        if Age.Map.is_empty t.envs then false
        else begin
          let rev_dep_opt =
            List.fold_left
              (fun acc dep ->
                 let rev_dep = Rev_deps.get rev_deps dep in
                 match acc with
                 | None -> Some rev_dep
                 | Some acc -> Some (Dependency.Set.inter acc rev_dep))
              None deps
          in
          let rev_dep =
            match rev_dep_opt with
            | None ->
                failwith "Origin_range_tbl.is_origin_empty: invalid origin"
            | Some rev_dep -> rev_dep
          in
          let matching = Dependency.Set.inter rev_dep t.dep_keys in
          Dependency.Set.is_empty matching
        end
    | Origin.Environment age ->
        match Age.Map.max_binding t.envs with
        | exception Not_found -> true
        | (max, _) -> Age.less_than max age

  let is_completely_empty t =
    Age.Map.is_empty t.envs
    && Dependency.Set.is_empty t.dep_keys

end

module Height = struct

  include Natural.Make_no_zero()

  let hidden_name name =
    if name <> "" && name.[0] = '_' then true
    else
      try
        for i = 1 to String.length name - 2 do
          if name.[i] = '_' && name.[i + 1] = '_' then
            raise Exit
        done;
        false
      with Exit -> true

  let hidden_ident id =
    if !Clflags.unsafe_string && Ident.same id Predef.ident_bytes then true
    else hidden_name (Ident.name id)

  let measure_name name =
    if hidden_name name then maximum
    else one

  let measure_ident id =
    if hidden_ident id then maximum
    else one

  let rec measure_path = function
    | Path.Pident id ->
        measure_ident id
    | Path.Pdot(p, name, _) ->
        if hidden_name name then maximum
        else succ (measure_path p)
    | Path.Papply(p1, p2) ->
        plus (measure_path p1) (measure_path p2)

end

module Todo = struct

  module Item = struct

    type t =
      | Base of Diff.Item.t
      | Children of Module.t * Path.t
      | Forward of Ident.t * Origin.t

    let pp ppf = function
      | Base(Diff.Item.Type(id, _, _)) ->
          Format.fprintf ppf "Base(Type(%a))"
            IdentOps.print id
      | Base(Diff.Item.Module_type(id, _, _)) ->
          Format.fprintf ppf "Base(Module_type(%a))"
            IdentOps.print id
      | Base(Diff.Item.Module(id, _, _)) ->
          Format.fprintf ppf "Base(Module(%a))"
            IdentOps.print id
      | Children(_, path) ->
          Format.fprintf ppf "Children(%a)"
            PathOps.print path
      | Forward(id, _) ->
          Format.fprintf ppf "Forward(%a)"
            IdentOps.print id
  end

  type t =
    { mutable table : Item.t Origin_range_tbl.t Height.Array.t }

  let add_diff_item graph rev_deps tbl item =
    let origin = Diff.Item.origin graph item in
    match Diff.Item.previous graph item with
    | None ->
        trace "Adding todo item %a to %a\n%!"
          Item.pp (Item.Base item) Origin.pp origin;
        Origin_range_tbl.add rev_deps origin (Item.Base item) tbl;
    | Some prev ->
        let id = Diff.Item.id graph item in
        trace "Adding todo item %a to %a\n%!"
          Item.pp (Item.Forward(id, prev)) Origin.pp origin;
        Origin_range_tbl.add rev_deps origin (Item.Forward(id, prev)) tbl

  let create graph rev_deps diff =
    let tbl = Origin_range_tbl.create () in
    List.iter (add_diff_item graph rev_deps tbl) diff;
    let table = Height.Array.singleton tbl in
    { table }

  let get_table t height =
    if not (Height.Array.contains t.table height) then begin
      t.table <- Height.Array.extend t.table height
                   (fun _ -> Origin_range_tbl.create ());
    end;
    Height.Array.get t.table height

  let get_table_opt t height =
    if Height.Array.contains t.table height then
      Some (Height.Array.get t.table height)
    else None

  let retract_empty t =
    let rec loop height =
      match Height.pred height with
      | None ->
          trace "Retracting todo list to length 0\n%!";
          t.table <- Height.Array.empty
      | Some prev ->
          let tbl = Height.Array.get t.table prev in
          if Origin_range_tbl.is_completely_empty tbl then loop prev
          else begin
            trace "Retracting todo list to length %a\n%!"
              Height.pp prev;
            t.table <- Height.Array.retract t.table height
          end
    in
    match Height.Array.last t.table with
    | None -> ()
    | Some last ->
      let tbl = Height.Array.get t.table last in
      if Origin_range_tbl.is_completely_empty tbl then loop last
      else ()

  let add graph rev_deps t diff =
    let tbl = get_table t Height.one in
    List.iter (add_diff_item graph rev_deps tbl) diff

  let add_children graph rev_deps t height md path =
    let height = Height.succ height in
    let tbl = get_table t height in
    let origin = Module.origin graph md in
    trace "Adding todo item %a to %a\n%!"
      Item.pp (Item.Children(md, path)) Origin.pp origin;
    Origin_range_tbl.add rev_deps origin (Item.Children(md, path)) tbl

  let add_next_forward rev_deps t height origin id prev =
    let height = Height.succ height in
    let tbl = get_table t height in
    trace "Adding todo item %a to %a\n%!"
      Item.pp (Item.Forward(id, prev)) Origin.pp origin;
    Origin_range_tbl.add rev_deps origin (Item.Forward(id, prev)) tbl

  let rec is_empty_from rev_deps t height origin =
    match get_table_opt t height with
    | None -> true
    | Some tbl ->
        Origin_range_tbl.is_origin_empty rev_deps origin tbl
        && is_empty_from rev_deps t (Height.succ height) origin

  let pop rev_deps t height origin =
    match get_table_opt t height with
    | None ->
        retract_empty t;
        None
    | Some tbl ->
      match Origin_range_tbl.pop rev_deps origin tbl with
      | [] ->
          let empty_from =
            is_empty_from rev_deps t (Height.succ height) origin
          in
          if not empty_from then Some []
          else begin
            retract_empty t;
            None
          end
      | _ :: _ as todo -> Some todo

end

module Forward_path_map : sig

  type t

  val empty : t

  val add : t -> Sort.t -> Path.t -> Path.t -> t

  val find : t -> Path.t -> Path.t list

  val union : t -> t -> t

  val forward : (Path.t -> Path.t -> unit) -> t -> Ident.t -> unit

  val print : Format.formatter -> t -> unit

end = struct

  type t =
    { paths : Path.t list Path_map.t;
      forward : Path_set.t Ident_map.t; }

  let empty =
    { paths = Path_map.empty;
      forward = Ident_map.empty; }

  let add t sort path data =
    let paths = t.paths in
    let prev =
      match Path_map.find path paths with
      | prev -> prev
      | exception Not_found -> []
    in
    let paths = Path_map.add path (data :: prev) paths in
    let forward = t.forward in
    let forward =
      match sort with
      | Sort.Defined -> forward
      | Sort.Declared ids ->
          Ident_set.fold
            (fun id acc ->
               let prev =
                 match Ident_map.find id t.forward with
                 | prev -> prev
                 | exception Not_found -> Path_set.empty
               in
               Ident_map.add id (Path_set. add path prev) acc)
            ids forward
    in
    { paths; forward }

  let find t path =
    Path_map.find path t.paths

  let union t1 t2 =
    let choose_right _ _ data2 = Some data2 in
    let paths =
      Path_map.union choose_right t1.paths t2.paths
    in
    let forward =
      Ident_map.union
        (fun _ pset1 pset2 -> Some (Path_set.union pset1 pset2))
        t1.forward t2.forward
    in
    { paths; forward }

  let forward f t id =
    match Ident_map.find id t.forward with
    | exception Not_found -> ()
    | pset ->
        Path_set.iter
          (fun path ->
             match find t path with
             | exception Not_found -> ()
             | paths -> List.iter (f path) paths)
          pset

  let print ppf t =
    Path_map.print
      (Format.pp_print_list ~pp_sep:Format.pp_print_space PathOps.print)
      ppf t.paths

end

module Origin_tbl = Hashtbl.Make(Origin)

module History : sig

  module Stamp : Natural.S

  module Revision : sig

    type t

    val stamp : t -> Stamp.t

    val diff : t ->  Diff.t

    val rev_deps : t -> Rev_deps.t

    val next : t -> t option

  end

  type t

  val init : Rev_deps.t ->  Diff.t -> t

  val head : t -> Revision.t

  val commit : t -> Rev_deps.t ->  Diff.t -> unit

end = struct

  module Stamp = Natural.Make()

  module Revision = struct

    type t =
      { stamp : Stamp.t;
        diff :  Diff.t;
        rev_deps : Rev_deps.t;
        mutable next : t option; }

    let stamp t = t.stamp

    let diff t = t.diff

    let rev_deps t = t.rev_deps

    let next t = t.next

  end

  type t =
    { mutable head : Revision.t; }

  let init rev_deps diff =
    let stamp = Stamp.zero in
    let next = None in
    let head = { Revision.stamp; diff; rev_deps; next } in
    { head }

  let head t = t.head

  let commit t rev_deps diff =
    let head = t.head in
    let stamp = Stamp.succ head.Revision.stamp in
    let next = None in
    let rev = { Revision.stamp; diff; rev_deps; next } in
    head.Revision.next <- Some rev;
    t.head <- rev

end

module Shortest = struct

  module Section = struct

    type t =
      { mutable types : Forward_path_map.t;
        mutable module_types : Forward_path_map.t;
        mutable modules : Forward_path_map.t; }

    let create () =
      let types = Forward_path_map.empty in
      let module_types = Forward_path_map.empty in
      let modules = Forward_path_map.empty in
      { types; module_types; modules }

    let add_type graph t typ path =
      let canonical = Type.path graph typ in
      let sort = Type.sort graph typ in
      t.types <- Forward_path_map.add t.types sort canonical path

    let add_module_type graph t mty path =
      let canonical = Module_type.path graph mty in
      let sort = Module_type.sort graph mty in
      t.module_types <- Forward_path_map.add t.module_types sort canonical path

    let add_module graph t md path =
      let canonical = Module.path graph md in
      let sort = Module.sort graph md in
      t.modules <- Forward_path_map.add t.modules sort canonical path

    let merge t parent =
      t.types <- Forward_path_map.union parent.types t.types;
      t.module_types <- Forward_path_map.union parent.module_types t.module_types;
      t.modules <- Forward_path_map.union parent.modules t.modules

    let forward ~type_ ~module_type ~module_ t id =
      Forward_path_map.forward type_ t.types id;
      Forward_path_map.forward module_type t.module_types id;
      Forward_path_map.forward module_ t.modules id

    let _print_sort ppf = function
      | Sort.Defined ->
          Format.fprintf ppf "Defined"
      | Sort.Declared ids ->
          Format.fprintf ppf "Declared(%a)"
            Ident_set.print ids

    let find_type graph t typ =
      let canonical = Type.path graph typ in
      trace "Looking in %a for %a\n%!"
        Forward_path_map.print t.types PathOps.print canonical;
      Forward_path_map.find t.types canonical

    let find_module_type graph t mty =
      let canonical = Module_type.path graph mty in
      Forward_path_map.find t.module_types canonical

    let find_module graph t md =
      let canonical = Module.path graph md in
      Forward_path_map.find t.modules canonical

  end

  module Sections = struct

    type range =
      | Until of Height.t
      | All

    type versioning =
      | Unversioned
      | Initialisation of History.Stamp.t
      | Completion of History.Stamp.t

    type t =
      { mutable sections : Section.t Height.Array.t;
        mutable initialised : range;
        mutable completed : range;
        mutable versioning : versioning; }

    let create age origin =
      let sections = Height.Array.empty in
      let completed = Until Height.one in
      let initialised, versioning =
        if Age.equal age Age.zero then begin
          All, Completion History.Stamp.zero
        end else begin
          match origin with
          | Origin.Environment age' ->
              let initialised =
                if Age.less_than_or_equal age age' then All
                else Until Height.one
              in
              initialised, Unversioned
          | Origin.Dependency _ | Origin.Dependencies _ ->
              Until Height.one, Initialisation History.Stamp.zero
        end
      in
      { sections; initialised; completed; versioning; }

    let update t stamp =
      match t.versioning with
      | Unversioned -> ()
      | Initialisation initialised ->
          if History.Stamp.less_than initialised stamp then begin
            t.initialised <- Until Height.one;
            t.versioning <- Initialisation stamp
          end
      | Completion completed ->
          if History.Stamp.less_than completed stamp then begin
            t.completed <- Until Height.one;
            t.versioning <- Completion stamp
          end

    let expand t height =
      let sections = t.sections in
      if not (Height.Array.contains sections height) then begin
        let sections =
          Height.Array.extend sections height
            (fun _ -> Section.create ())
        in
        t.sections <- sections;
        sections
      end else begin
        sections
      end

    let is_initialised t height =
      match t.initialised with
      | All -> true
      | Until until -> Height.less_than height until

    let set_initialised t height =
      match t.initialised with
      | All ->
          failwith "Section.set_initialised: already initialised"
      | Until until ->
          if not (Height.equal until height) then begin
            if Height.less_than until height then
              failwith "Section.set_initialised: initialised early"
            else
              failwith "Section.set_initialised: already initialised"
          end;
          t.initialised <- Until (Height.succ until)

    let set_initialised_from t height =
      match t.initialised with
      | All ->
          failwith "Section.set_initialised: already initialised"
      | Until until ->
          if not (Height.equal until height) then begin
            if Height.less_than until height then
              failwith "Section.set_initialised: initialised early"
            else
              failwith "Section.set_initialised: already initialised"
          end;
          t.initialised <- All

    let is_completed t height =
      match t.completed with
      | All -> true
      | Until until -> Height.less_than height until

    let set_completed t height =
      match t.completed with
      | All ->
          failwith "Section.set_completed: already completed"
      | Until until ->
          if not (Height.equal until height) then begin
            if Height.less_than until height then
              failwith "Section.set_completed: completed early"
            else
              failwith "Section.set_completed: already completed"
          end;
          t.completed <- Until (Height.succ until)

    let set_completed_from t height =
      match t.completed with
      | All ->
          failwith "Section.set_completed: already completed"
      | Until until ->
          if not (Height.equal until height) then begin
            if Height.less_than until height then
              failwith "Section.set_completed: completed early"
            else
              failwith "Section.set_completed: already completed"
          end;
          t.completed <- All

    let is_finished t =
      match t.initialised, t.completed with
      | All, All -> true
      | _, _ -> false

    let get t height =
      let sections = t.sections in
      if Height.Array.contains sections height then
        Some (Height.Array.get sections height)
      else None

    let check_initialised t height =
      match t.initialised with
      | All -> ()
      | Until until ->
          if not (Height.less_than height until) then
            failwith "Sections: section not initialised"

    let check_completed t height =
      match t.completed with
      | All -> ()
      | Until until ->
          if not (Height.less_than height until) then
            failwith "Sections: section not completed"

    let check_versions t parent =
      match t.versioning, parent.versioning with
      | Unversioned, _ | _, Unversioned -> ()
      | (Completion stamp | Initialisation stamp),
        (Completion parent_stamp | Initialisation parent_stamp) ->
          if not (History.Stamp.equal stamp parent_stamp) then
            failwith "Sections: version mismatch"

    let initialise t height parent =
      check_versions t parent;
      check_completed parent height;
      match get parent height with
      | Some parent ->
          let sections = expand t height in
          let section = Height.Array.get sections height in
          Section.merge section parent;
          set_initialised t height
      | None ->
          if is_finished parent then
            set_initialised_from t height
          else
            set_initialised t height

    let add_type graph t height typ path =
      let sections = expand t height in
      let section = Height.Array.get sections height in
      Section.add_type graph section typ path

    let add_module_type graph t height mty path =
      let sections = expand t height in
      let section = Height.Array.get sections height in
      Section.add_module_type graph section mty path

    let add_module graph t height md path =
      let sections = expand t height in
      let section = Height.Array.get sections height in
      Section.add_module graph section md path

    (* returns [true] if there might be forward paths at a greater height. *)
    let forward ~type_ ~module_type ~module_ t height id =
      let all =
        match t.initialised with
        | All -> true
        | Until until ->
            if not (Height.less_than height until) then
              failwith "Sections.forward: section not initialised";
            false
      in
      match get t height with
      | Some section ->
          Section.forward ~type_ ~module_type ~module_ section id;
          true
      | None -> not all

    type result =
      | Not_found_here
      | Not_found_here_or_later
      | Found of Path.t

    let rec get_visible_type graph = function
      | [] -> None
      | path :: rest ->
          let visible = Graph.is_type_path_visible graph path in
          if visible then Some path
          else get_visible_type graph rest

    let rec get_visible_module_type graph = function
      | [] -> None
      | path :: rest ->
          let visible = Graph.is_module_type_path_visible graph path in
          if visible then Some path
          else get_visible_module_type graph rest

    let rec get_visible_module graph = function
      | [] -> None
      | path :: rest ->
          let visible = Graph.is_module_path_visible graph path in
          if visible then Some path
          else get_visible_module graph rest

    let find_type graph t height typ =
      check_initialised t height;
      check_completed t height;
      match get t height with
      | Some section -> begin
          match Section.find_type graph section typ with
          | exception Not_found -> Not_found_here
          | paths -> begin
              match get_visible_type graph paths with
              | None -> Not_found_here
              | Some path -> Found path
            end
        end
      | None ->
          if is_finished t then Not_found_here_or_later
          else Not_found_here

    let find_module_type graph t height mty =
      check_initialised t height;
      check_completed t height;
      match get t height with
      | Some section -> begin
          match Section.find_module_type graph section mty with
          | exception Not_found -> Not_found_here
          | paths -> begin
              match get_visible_module_type graph paths with
              | None -> Not_found_here
              | Some path -> Found path
            end
        end
      | None ->
          if is_finished t then Not_found_here_or_later
          else Not_found_here

    let find_module graph t height md =
      check_initialised t height;
      check_completed t height;
      match get t height with
      | Some section -> begin
          match Section.find_module graph section md with
          | exception Not_found -> Not_found_here
          | paths -> begin
              match get_visible_module graph paths with
              | None -> Not_found_here
              | Some path -> Found path
            end
        end
      | None ->
          if is_finished t then Not_found_here_or_later
          else Not_found_here

  end

  type basis

  type env

  type _ kind =
    | Basis :
        { history : History.t; }
      -> basis kind
    | Env :
        { mutable revision : History.Revision.t;
          parent : 'a t;
          age : Age.t; }
      -> env kind

  and 'a t =
    { kind : 'a kind;
      mutable graph : Graph.t;
      sections: Sections.t Origin_tbl.t;
      todos: Todo.t; }

  let age (type k) (t : k t) =
    match t.kind with
    | Basis _ -> Age.zero
    | Env { age; _ } -> age

  let revision (type k) (t : k t) =
    match t.kind with
    | Basis { history } -> History.head history
    | Env { revision; _ } -> revision

  let stamp t =
    History.Revision.stamp (revision t)

  let rev_deps t =
    History.Revision.rev_deps (revision t)

  let update (type kind) (t : kind t) =
    match t.kind with
    | Basis _ -> ()
    | Env ({ revision } as e) ->
        let rec loop graph revision =
          let next = History.Revision.next revision in
          match next with
          | None -> revision, graph
          | Some revision ->
              let diff = History.Revision.diff revision in
              trace "Merging updates from basis in Env(%a) (%i additions)\n%!"
                Age.pp (age t) (List.length diff);
              let graph = Graph.merge graph diff in
              loop graph revision
        in
        let revision, graph = loop t.graph revision in
        t.graph <- graph;
        e.revision <- revision

  let basis rev_deps components =
    let graph, diff = Graph.add Graph.empty components in
    let history = History.init rev_deps diff in
    let kind = Basis { history } in
    let sections = Origin_tbl.create 0 in
    let todos = Todo.create graph rev_deps diff in
    { kind; graph; sections; todos }

  let env parent desc =
    update parent;
    let age = Age.succ (age parent) in
    let origin = Origin.Environment age in
    let components =
      List.map
        (fun desc ->
           match desc with
           | Desc.Type(id, desc, conc) ->
               Component.Type(origin, id, desc, conc)
           | Desc.Module_type(id, desc, conc) ->
               Component.Module_type(origin, id, desc, conc)
           | Desc.Module(id, desc, conc) ->
               Component.Module(origin, id, desc, conc)
           | Desc.Declare_type id ->
               Component.Declare_type(origin, id)
           | Desc.Declare_module_type id ->
               Component.Declare_module_type(origin, id)
           | Desc.Declare_module id ->
               Component.Declare_module(origin, id))
        desc
    in
    let graph, diff = Graph.add parent.graph components in
    let revision = revision parent in
    let kind = Env { revision; parent; age } in
    let sections = Origin_tbl.create 0 in
    let rev_deps = rev_deps parent in
    let todos = Todo.create graph rev_deps diff in
    { kind; graph; sections; todos }

  let mutate (t : basis t) rev_deps components =
    let graph, diff = Graph.add t.graph components in
    let Basis { history } = t.kind in
    History.commit history rev_deps diff;
    t.graph <- graph;
    Todo.add graph rev_deps t.todos diff

  let sections t origin =
    match Origin_tbl.find t.sections origin with
    | exception Not_found ->
        let sections = Sections.create (age t) origin in
        Origin_tbl.add t.sections origin sections;
        sections
    | sections -> sections

  let process_type t height path typ =
    trace "Processing type path %a\n%!" PathOps.print path;
    let canonical_path = Type.path t.graph typ in
    if not (Path.same canonical_path path) then begin
      let origin = Type.origin t.graph typ in
      let sections = sections t origin in
      Sections.add_type t.graph sections height typ path
    end

  let process_module_type t height path mty =
    trace "Processing module type path %a\n%!" PathOps.print path;
    let canonical_path = Module_type.path t.graph mty in
    if not (Path.same canonical_path path) then begin
      let origin = Module_type.origin t.graph mty in
      let sections = sections t origin in
      Sections.add_module_type t.graph sections height mty path
    end

  let process_module t height path md =
    trace "Processing module path %a\n%!" PathOps.print path;
    let canonical_path = Module.path t.graph md in
    if not (Path.same canonical_path path) then begin
      let origin = Module.origin t.graph md in
      let sections = sections t origin in
      Sections.add_module t.graph sections height md path;
    end;
    Todo.add_children t.graph (rev_deps t) t.todos height md path

  let process_children t height path md =
    trace "Processing children of %a\n%!" PathOps.print path;
    let types =
      match Module.types t.graph md with
      | Some types -> types
      | None -> String_map.empty
    in
    let module_types =
      match Module.module_types t.graph md with
      | Some module_types -> module_types
      | None -> String_map.empty
    in
    let modules =
      match Module.modules t.graph md with
      | Some modules -> modules
      | None -> String_map.empty
    in
    String_map.iter
      (fun name typ ->
         if not (Height.hidden_name name) then begin
           let path = Path.Pdot(path, name, 0) in
           process_type t height path typ
         end)
      types;
    String_map.iter
      (fun name mty ->
         if not (Height.hidden_name name) then begin
           let path = Path.Pdot(path, name, 0) in
           process_module_type t height path mty
         end)
      module_types;
    String_map.iter
      (fun name md ->
         if not (Height.hidden_name name) then begin
           let path = Path.Pdot(path, name, 0) in
           process_module t height path md
         end)
      modules

  let rec process : 'k . 'k t -> _ =
    fun t origin height ->
      let todo = Todo.pop (rev_deps t) t.todos height origin in
      match todo with
      | None -> true
      | Some items ->
          List.iter
            (function
              | Todo.Item.Base (Diff.Item.Type(id, typ, _)) ->
                  if not (Height.hidden_ident id) then begin
                    let path = Path.Pident id in
                    process_type t height path typ
                  end
              | Todo.Item.Base (Diff.Item.Module_type(id, mty, _)) ->
                  if not (Height.hidden_ident id) then begin
                    let path = Path.Pident id in
                    process_module_type t height path mty
                  end
              | Todo.Item.Base (Diff.Item.Module(id, md, _)) ->
                  if not (Height.hidden_ident id) then begin
                    let path = Path.Pident id in
                    process_module t height path md
                  end
              | Todo.Item.Children(md, path) ->
                  process_children t height path md
              | Todo.Item.Forward(id, prev) ->
                  process_forward t origin height id prev)
            items;
            false

  and process_forward : 'k . 'k t -> _ =
    fun t origin height id prev ->
      trace "Processing forwarded paths from %a\n%!"
        IdentOps.print id;
      let sections = init t prev height in
      let more =
        Sections.forward sections height id
          ~type_:(fun canon path ->
            let typ = Graph.find_type t.graph canon in
            trace "Forwarding %a type path from %a to %a\n%!"
              PathOps.print path PathOps.print canon
              PathOps.print (Type.path t.graph typ);
            process_type t height path typ)
          ~module_type:(fun canon path ->
            let mty = Graph.find_module_type t.graph canon in
            trace "Forwarding %a module type path from %a to %a\n%!"
              PathOps.print path PathOps.print canon
              PathOps.print (Module_type.path t.graph mty);
            process_module_type t height path mty)
          ~module_:(fun canon path ->
            let md = Graph.find_module t.graph canon in
            trace "Forwarding %a module path from %a to %a\n%!"
              PathOps.print path PathOps.print canon
              PathOps.print (Module.path t.graph md);
            process_module t height path md);
      in
      if more then
        Todo.add_next_forward (rev_deps t) t.todos height origin id prev

  and initialise : type k. k t -> _ =
    fun t sections origin height ->
      if not (Sections.is_initialised sections height) then begin
        begin match Height.pred height with
        | None -> ()
        | Some pred -> initialise t sections origin pred
        end;
        trace "Initialising origin %a at height %a in Env(%a)\n%!"
          Origin.pp origin Height.pp height
          Age.pp (age t);
        let parent =
          match t.kind with
          | Basis _ -> assert false
          | Env { parent; _ } ->
              update parent;
              force parent origin height
        in
        Sections.initialise sections height parent
      end

  and init : 'k . 'k t -> _ =
    fun t origin height ->
      let sections = sections t origin in
      Sections.update sections (stamp t);
      initialise t sections origin height;
      sections

  and complete : 'k. 'k t -> _ =
    fun t sections origin height ->
      if not (Sections.is_completed sections height) then begin
        begin match Height.pred height with
        | None -> ()
        | Some pred -> ignore (complete t sections origin pred)
        end;
        trace "Forcing origin %a at height %a in Env(%a)\n%!"
          Origin.pp origin Height.pp height Age.pp (age t);
        let finished = process t origin height in
        if finished then Sections.set_completed_from sections height
        else Sections.set_completed sections height
      end

  and force : 'k. 'k t -> _ =
    fun t origin height ->
      let sections = sections t origin in
      Sections.update sections (stamp t);
      initialise t sections origin height;
      complete t sections origin height;
      sections

  module Search = struct

    type 'a shortest = 'a t

    type _ kind =
      | Type : Type.t kind
      | Module_type : Module_type.t kind
      | Module : Module.t kind

    type suffix =
      { names : string list;
        height : Height.t; }

    type 'a t =
      | Simple of
          { kind : 'a kind;
            node : 'a;
            origin : Origin.t;
            best : Path.t;
            min: Height.t;
            max: Height.t;
            finished : bool; }
      | Application of
          { kind : 'a kind;
            node : 'a;
            origin : Origin.t;
            best : Path.t;
            min: Height.t;
            max: Height.t;
            func : Module.t t;
            arg : Module.t t;
            suffix : suffix option;
            func_first : bool;
            searched : bool;
            finished : bool; }

    let min_height = function
      | Simple { min; _ } -> min
      | Application { min; _ } -> min

    let finished = function
      | Simple { finished; _ } -> finished
      | Application { finished; _ } -> finished

    let best = function
      | Simple { best; _ } -> best
      | Application { best; _ } -> best

    let min_application fst snd suffix =
      let base = Height.plus (min_height fst) (min_height snd) in
      match suffix with
      | None -> base
      | Some { height; _ } -> Height.plus base height

    let path_application fst snd suffix =
      let base = Path.Papply(best fst, best snd) in
      match suffix with
      | None -> base
      | Some { names; _ } ->
          List.fold_left
            (fun acc name -> Path.Pdot(acc, name, 0))
            base names

    let create (type k) shortest (kind : k kind) path =
      let rec loop :
        type k. k kind -> k -> Origin.t -> Path.t ->
          Height.t -> string list -> Path.t -> k t =
        fun kind node origin best max suffix path ->
          match path with
          | Path.Pident _ ->
              let min = Height.one in
              let finished = false in
              Simple
                { kind; node; origin; best; min; max; finished; }
          | Path.Pdot(p, name, _) ->
              loop kind node origin best max (name :: suffix) p
          | Path.Papply(func, arg) ->
              let graph = shortest.graph in
              let func_md = Graph.find_module graph func in
              let func_max = Height.measure_path func in
              let func_origin = Module.origin graph func_md in
              let func =
                loop Module func_md func_origin func func_max [] func
              in
              let arg_md = Graph.find_module graph arg in
              let arg_max = Height.measure_path arg in
              let arg_origin = Module.origin graph arg_md in
              let arg =
                loop Module arg_md arg_origin arg arg_max [] arg
              in
              let func_first =
                Rev_deps.before (rev_deps shortest) arg_origin func_origin
              in
              let finished = false in
              let suffix =
                match suffix with
                | [] -> None
                | fst :: rest ->
                  let names = suffix in
                  let height =
                    List.fold_left
                      (fun acc name ->
                         Height.plus acc (Height.measure_name name))
                      (Height.measure_name fst) rest
                  in
                  Some { names; height }
              in
              let searched, min =
                match kind with
                | Type ->
                    let searched = false in
                    let min = Height.one in
                    searched, min
                | Module_type ->
                    let searched = false in
                    let min = Height.one in
                    searched, min
                | Module ->
                    (* There are no module aliases containing extended paths *)
                    let searched = true in
                    let min = min_application func arg suffix in
                    searched, min
              in
              Application
                { kind; node; origin; best; min; max;
                  func; arg; suffix; func_first; searched; finished }
      in
      let graph = shortest.graph in
      let node, canonical_path, origin, max =
        (match kind with
        | Type ->
            let typ = Graph.find_type graph path in
            let canonical_path = Type.path graph typ in
            let origin = Type.origin graph typ in
            let max =
              let visible =
                Graph.is_type_path_visible graph canonical_path
              in
              if visible then Height.measure_path canonical_path
              else Height.maximum
            in
            typ, canonical_path, origin, max
        | Module_type ->
            let mty = Graph.find_module_type graph path in
            let canonical_path = Module_type.path graph mty in
            let origin = Module_type.origin graph mty in
            let max =
              let visible =
                Graph.is_module_type_path_visible graph canonical_path
              in
              if visible then Height.measure_path canonical_path
              else Height.maximum
            in
            mty, canonical_path, origin, max
        | Module ->
            let md = Graph.find_module graph path in
            let canonical_path = Module.path graph md in
            let origin = Module.origin graph md in
            let max =
              let visible =
                Graph.is_module_path_visible graph canonical_path
              in
              if visible then Height.measure_path canonical_path
              else Height.maximum
            in
            md, canonical_path, origin, max : k * _ * _ * _)
      in
      loop kind node origin canonical_path max [] canonical_path

    (* TODO remove this when tracing goes *)
    let path (type k) graph (kind : k kind) (node : k) =
      match kind with
      | Type -> Type.path graph node
      | Module_type -> Module_type.path graph node
      | Module -> Module.path graph node

    let find (type k) shortest origin height (kind : k kind) (node : k) =
      trace "Searching for %a from %a at height %a in Env(%a)\n%!"
        PathOps.print (path shortest.graph kind node)
        Origin.pp origin Height.pp height
        Age.pp (age shortest);
      let sections = force shortest origin height in
      match kind with
      | Type ->
          Sections.find_type shortest.graph sections height node
      | Module_type ->
          Sections.find_module_type shortest.graph sections height node
      | Module ->
          Sections.find_module shortest.graph sections height node

    let rec step : type k . _ shortest -> k t -> k t =
      fun shortest search ->
        if finished search then search
        else begin
          match search with
          | Simple r -> begin
              match find shortest r.origin r.min r.kind r.node with
              | Sections.Not_found_here ->
                  if Height.equal r.min r.max then
                    Simple { r with finished = true }
                  else
                    Simple { r with min = Height.succ r.min }
              | Sections.Not_found_here_or_later ->
                  Simple { r with finished = true; min = r.max }
              | Sections.Found path ->
                  let best = path in
                  let max = r.min in
                  let finished = true in
                  Simple { r with best; max; finished }
            end
         | Application r ->
              let try_app searched =
                let fst, snd =
                  if r.func_first then r.func, r.arg
                  else r.arg, r.func
                in
                let fst, snd =
                  let not_found =
                    Height.less_than
                      r.min (min_application fst snd r.suffix)
                  in
                  if not_found then fst, snd
                  else begin
                    let fst = step shortest fst in
                    let not_found =
                      Height.less_than
                        r.min (min_application fst snd r.suffix)
                    in
                    if not_found then fst, snd
                    else fst, step shortest snd
                  end
                in
                let func, arg =
                  if r.func_first then fst, snd
                  else snd, fst
                in
                let not_found =
                  Height.less_than
                    r.min (min_application func arg r.suffix)
                in
                if not_found then begin
                  let finished = searched && finished func && finished arg in
                  let min = if finished then r.max else Height.succ r.min in
                  Application
                    { r with func; arg; min; searched; finished }
                end else begin
                  let best = path_application func arg r.suffix in
                  let max = r.min in
                  let finished = true in
                  Application
                    { r with best; func; arg; max; searched; finished }
                end
              in
              if r.searched then try_app true
              else begin
                match find shortest r.origin r.min r.kind r.node with
                | Sections.Not_found_here ->
                    try_app (Height.equal r.min r.max)
                | Sections.Not_found_here_or_later ->
                    try_app true
                | Sections.Found path ->
                    let best = path in
                    let max = r.min in
                    let finished = true in
                    Application { r with best; max; finished }
              end
        end

    let rec perform shortest search =
      if finished search then best search
      else perform shortest (step shortest search)

  end

  let find_type t path =
    update t;
    let search = Search.create t Search.Type path in
    Search.perform t search

  let find_module_type t path =
    update t;
    let search = Search.create t Search.Module_type path in
    Search.perform t search

  let find_module t path =
    update t;
    let search = Search.create t Search.Module path in
    Search.perform t search

end

module Basis = struct

  type modification =
    | Add of
        { name : string; }
    | Load of
        { name : string;
          depends : string list;
          desc : Desc.Module.t; }

  type t =
    { mutable next_dep : Dependency.t;
      mutable pending : modification list;
      mutable assignment : Dependency.t String_map.t;
      mutable rev_deps : Rev_deps.t;
      mutable shortest : Shortest.basis Shortest.t option; }

  let create () =
    { next_dep = Dependency.zero;
      pending = [];
      assignment = String_map.empty;
      rev_deps = Rev_deps.empty;
      shortest = None; }

  let rec update_assignments t = function
    | [] -> ()
    | Load _ :: rest -> update_assignments t rest
    | Add { name } :: rest ->
      if not (String_map.mem name t.assignment) then begin
        t.assignment <- String_map.add name t.next_dep t.assignment;
        t.next_dep <- Dependency.succ t.next_dep
      end;
      update_assignments t rest

  let update_rev_deps t mods =
    let rec loop = function
      | [] -> ()
      | Add _ :: rest -> loop rest
      | Load { name; depends; _ } :: rest ->
        let index = String_map.find name t.assignment in
        List.iter
          (fun dep_name ->
             let dep_index = String_map.find dep_name t.assignment in
             Rev_deps.add t.rev_deps dep_index index)
          depends;
        loop rest
    in
    trace "Extending rev_deps to %a\n%!"
        Dependency.pp t.next_dep;
    let rev_deps = Rev_deps.extend_up_to t.rev_deps t.next_dep in
    t.rev_deps <- rev_deps;
    loop mods

  let update_shortest t mods =
    let components =
      List.map
        (function
          | Add { name } ->
              let index = String_map.find name t.assignment in
              let origin = Origin.Dependency index in
              let id = Ident.create_persistent name in
              Component.Declare_module(origin, id)
          | Load { name; desc; _ } ->
              let index = String_map.find name t.assignment in
              let origin = Origin.Dependency index in
              let id = Ident.create_persistent name in
              Component.Module(origin, id, desc, true))
        mods
    in
    match t.shortest with
    | None ->
        t.shortest <- Some (Shortest.basis t.rev_deps components)
    | Some shortest ->
        Shortest.mutate shortest t.rev_deps components

  let print_modification ppf = function
    | Add { name } -> Format.fprintf ppf "Add(%s)" name
    | Load { name } -> Format.fprintf ppf "Load(%s)" name

  let update t =
    match t.pending with
    | [] -> ()
    | _ :: _ as pending ->
      trace "Updating basis (%i pending): %a\n%!"
        (List.length pending) (Format.pp_print_list print_modification) pending;
      t.pending <- [];
      let mods = List.rev pending in
      update_assignments t mods;
      update_rev_deps t mods;
      update_shortest t mods

  let shortest t =
    update t;
    match t.shortest with
    | None ->
        let shortest = Shortest.basis t.rev_deps [] in
        t.shortest <- Some shortest;
        shortest
    | Some shortest -> shortest

  let add t name =
    t.pending <- Add { name } :: t.pending

  let load t name depends desc =
    t.pending <- Load { name; depends; desc } :: t.pending

end

type state =
  | Initial of Basis.t
  | Unforced of
      { parent : t;
        desc : Desc.t list Lazy.t; }
  | Forced of
      { basis : Basis.t;
        shortest : Shortest.env Shortest.t; }

and t = state ref

let rec force t =
  match !t with
  | Initial _ | Forced _ as state -> state
  | Unforced { parent; desc } ->
    let desc = Lazy.force desc in
    trace "Forcing environment (%i elements)\n%!"
      (List.length desc);
    let state =
      match force parent with
      | Unforced _ -> assert false
      | Initial basis ->
        let shortest = Shortest.env (Basis.shortest basis) desc in
        Forced { basis; shortest }
      | Forced { basis; shortest } ->
        let shortest = Shortest.env shortest desc in
        Forced { basis; shortest }
    in
    t := state;
    state

let initial basis = ref (Initial basis)

let add parent desc =
  ref (Unforced { parent; desc })

let find_type t path =
  trace "\n*********************************\n%!";
  trace "Finding type %a\n%!" PathOps.print path;
  let result =
    match force t with
    | Unforced _ -> assert false
    | Initial basis ->
        Basis.update basis;
        let shortest = Basis.shortest basis in
        Shortest.find_type shortest path
  | Forced { basis; shortest } ->
      Basis.update basis;
      Shortest.find_type shortest path
  in
  trace "Found %a\n%!" PathOps.print result;
  trace "*********************************\n%!";
  result


let find_module_type t path =
  trace "\n*********************************\n%!";
  trace "Finding module type %a\n%!" PathOps.print path;
  let result =
    match force t with
    | Unforced _ -> assert false
    | Initial basis ->
        Basis.update basis;
        let shortest = Basis.shortest basis in
        Shortest.find_module_type shortest path
    | Forced { basis; shortest } ->
        Basis.update basis;
        Shortest.find_module_type shortest path
  in
  trace "Found %a\n%!" PathOps.print result;
  trace "*********************************\n%!";
  result

let find_module t path =
  trace "\n*********************************\n%!";
  trace "Finding module %a\n%!" PathOps.print path;
  let result =
    match force t with
    | Unforced _ -> assert false
    | Initial basis ->
        Basis.update basis;
        let shortest = Basis.shortest basis in
        Shortest.find_module shortest path
    | Forced { basis; shortest } ->
        Basis.update basis;
        Shortest.find_module shortest path
  in
  trace "Found %a\n%!" PathOps.print result;
  trace "*********************************\n%!";
  result
