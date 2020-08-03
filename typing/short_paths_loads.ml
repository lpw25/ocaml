
module String_set = Set.Make(String)

module String_map = Map.Make(String)

module Dependency = Natural.Make()

module Time = Natural.Make()

module Origin = struct

  type t =
    | Dependency of Dependency.t
    | Dependencies of Dependency.t list
    | Environment of Time.t

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
        Environment (Time.max age1 age2)

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
    | Environment env1, Environment env2 -> Time.equal env1 env2

  let hash = Hashtbl.hash

end

(** [Rev_deps] keeps track of the reverse dependencies of each global module
    (represented as a [Dependency.t]).  *)
module Rev_deps : sig

  type t

  val create : unit -> t

  (** Implementation detail:
      [extend_up_to t dep] allocates space for storing dependencies
      information up to [dep]. *)
  val extend_up_to : t -> Dependency.t -> unit

  (** Get the transitive set of reverse dependencies of a module
      (note: a module is part of its own reverse dependencies).  *)
  val get : t -> Dependency.t -> Dependency.Set.t

  (* Register a concrete reverse dependency (target depends on source) *)
  val add : t -> source:Dependency.t -> target:Dependency.t -> unit

  (* Register an alias reverse dependency (target has an alias to source) *)
  val add_alias : t -> source:Dependency.t -> target:Dependency.t -> unit

  (* [before t o1 o2] is true if [o2] exists only in environments where [o1]
     has already been defined.
     This is the case if [o2] is a local definition made after [o1],
     or if [o2] comes from a global module that depends on [o1].

     Thus, [before] defines a total ordering on local definitions and
     a partial ordering on global dependencies. *)
  val before : t -> Origin.t -> Origin.t -> bool

end = struct

  (* Stamps are used to update lazily: each modification increases the stamp.
     Each dependency comes with the last stamp at which it has been updated.
     At query-time, the dependency set is refreshed if the stamp is out of
     date.
  *)
  module Stamp = Natural.Make()

  type item = {
    mutable set : Dependency.Set.t;
    (** output: reflexive & transitive closure of reverse dependencies *)
    mutable edges : Dependency.t list;
    (** input: immediate concrete reverse dependencies *)
    mutable alias_edges : Dependency.t list;
    (** input: immediate alias reverse dependencies *)
    mutable last : Stamp.t;
    (** last update time *)
  }

  type t =
    { mutable stamp : Stamp.t;
      mutable items : item Dependency.Array.t; }

  let create () =
    { stamp = Stamp.one;
      items = Dependency.Array.empty; }

  let extend_up_to t next =
    match Dependency.pred next with
    | None -> ()
    | Some curr ->
      if not (Dependency.Array.contains t.items curr) then begin
        let items =
          Dependency.Array.extend t.items curr
            (fun _ -> { set = Dependency.Set.empty;
                        edges = [];
                        alias_edges = [];
                        last = Stamp.zero; })
        in
        t.items <- items
      end

  let add t ~source ~target =
    let item = Dependency.Array.get t.items source in
    item.edges <- target :: item.edges;
    t.stamp <- Stamp.succ t.stamp

  let add_alias t ~source ~target =
    let item = Dependency.Array.get t.items source in
    item.alias_edges <- target :: item.alias_edges;
    t.stamp <- Stamp.succ t.stamp

  let update t dep item =
    if Stamp.less_than item.last t.stamp then begin
      (* Recompute closure *)
      let rec add_edges t item acc =
        let rec loop t acc added = function
          | [] ->
              List.fold_left
                (fun acc dep ->
                   let item = Dependency.Array.get t.items dep in
                   add_alias_edges t item acc)
                acc added
          | dep :: rest ->
              if Dependency.Set.mem dep acc then loop t acc added rest
              else begin
                let acc = Dependency.Set.add dep acc in
                let added = dep :: added in
                loop t acc added rest
              end
        in
        loop t acc [] item.edges
      and add_alias_edges t item acc =
        List.fold_left
          (fun acc dep ->
             if Dependency.Set.mem dep acc then acc
             else begin
               let acc = Dependency.Set.add dep acc in
               let item = Dependency.Array.get t.items dep in
               let acc = add_edges t item acc in
               add_alias_edges t item acc
             end)
          acc item.alias_edges
      in
      let set = Dependency.Set.singleton dep in
      let set = add_edges t item set in
      let set = add_alias_edges t item set in
      item.set <- set;
      item.last <- t.stamp
    end

  let get t dep =
    let item = Dependency.Array.get t.items dep in
    update t dep item;
    item.set

  let before t origin1 origin2 =
    let open Origin in
    match origin1, origin2 with
    | Environment age1, Environment age2 -> Time.less_than age1 age2
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

(* A history of the loaded dependencies. *)
module History : sig

  module Stamp : Natural.S

  module Revision : sig

    type t

    val stamp : t -> Stamp.t

    val loads : t -> Dependency.t list

    val next : t -> t option

    val initial : unit -> t

    val commit : t -> Dependency.t list -> t

  end

end = struct

  module Stamp = Natural.Make()

  module Revision = struct

    type t =
      { stamp : Stamp.t;
        loads :  Dependency.t list;
        mutable next : t option; }

    let stamp t = t.stamp

    let loads t = t.loads

    let next t = t.next

    let initial () =
      let stamp = Stamp.zero in
      let loads = [] in
      let next = None in
      { stamp; loads; next }

    let commit t loads =
      let stamp = Stamp.succ t.stamp in
      let next = None in
      let rev = { stamp; loads; next } in
      t.next <- Some rev;
      rev

  end

end

module Load = struct

  type t =
    { name : string;
      depends : string list;
      alias_depends : string list; }

end

module Basis = struct

  type t =
    { mutable next_dep : Dependency.t;
      mutable assignment : Dependency.t String_map.t;
      rev_deps : Rev_deps.t;
      mutable head : History.Revision.t; }

  let create () =
    { next_dep = Dependency.zero;
      assignment = String_map.empty;
      rev_deps = Rev_deps.create ();
      head = History.Revision.initial (); }

  let add_references t references =
    String_set.iter
      (fun name ->
         if not (String_map.mem name t.assignment) then begin
           t.assignment <- String_map.add name t.next_dep t.assignment;
           t.next_dep <- Dependency.succ t.next_dep
         end)
      references

  let add_loads t loads =
    Rev_deps.extend_up_to t.rev_deps t.next_dep;
    let loads =
      List.map
        (fun { Load.name; depends; alias_depends } ->
           let index = String_map.find name t.assignment in
           List.iter
             (fun dep_name ->
                let dep_index = String_map.find dep_name t.assignment in
                Rev_deps.add t.rev_deps ~source:dep_index ~target:index)
             depends;
           List.iter
             (fun dep_name ->
                let dep_index = String_map.find dep_name t.assignment in
                Rev_deps.add_alias t.rev_deps ~source:dep_index ~target:index)
             alias_depends;
           index)
        loads
    in
    t.head <- History.Revision.commit t.head loads

  let add t ~(references : String_set.t) ~(loads : Load.t list) =
    match loads, String_set.is_empty references with
    | [], true -> ()
    | _, _ ->
        add_references t references;
        add_loads t loads

  let head t = t.head

  let rev_deps t = t.rev_deps

  let find_dependency t name =
    String_map.find name t.assignment
end
