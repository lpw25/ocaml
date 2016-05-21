
type path =
  | Simple of string
  | Packed of string list * string
  | Project of path * string
  | ProjectOrSimple of path * string

type t = {
  root : string option;
  path : path;
}

let root { root } = root

let name { path } =
  match path with
  | Simple name -> name
  | Packed(_, name) -> name
  | Project(_, name) -> name
  | ProjectOrSimple(_, name) -> name

let simple ~name =
  { root = None;
    path = Simple name; }

let base ~root ~name =
  { root = Some root;
    path = Simple name; }

let project ~parent ~name =
  { root = parent.root;
    path = Project(parent.path, name); }

let project_or_simple ~parent ~name =
  { root = parent.root;
    path = ProjectOrSimple(parent.path, name); }

let dummy =
  { root = None;
    path = Simple "" }

let pack ~pack ~uname =
  let root =
    match uname.root with
    | Some _ -> failwith "Unit_name.pack: unit has root"
    | None -> pack.root
  in
  let pack =
    match pack.path with
    | Simple name -> [name]
    | Packed(packs, name) -> packs @ [name]
    | Project _ | ProjectOrSimple _ ->
        failwith "Unit_name.pack: pack is projected"
  in
  let path =
    match uname.path with
    | Simple name -> Packed(pack, name)
    | Packed(packs, name) -> Packed(pack @ packs, name)
    | Project _ | ProjectOrSimple _ ->
        failwith "Unit_name.pack: unit is projected"
  in
  { root; path; }

let packed uname =
  match uname.path with
  | Packed _ -> true
  | Simple _ | Project _ | ProjectOrSimple _ -> false

let prefix ~prefix ~uname =
  match uname.root with
  | Some _ -> uname
  | None ->
      let root = prefix.root in
      let rec loop = function
        | Simple name -> ProjectOrSimple(prefix.path, name)
        | Packed _ -> failwith "Unit_name.prefix: packed unit"
        | Project(parent, name) -> Project(loop parent, name)
        | ProjectOrSimple(parent, name) -> ProjectOrSimple(loop parent, name)
      in
      let path = loop uname.path in
      { root; path }

let of_string s = simple ~name:s

let equal_root r1 r2 =
  match r1, r2 with
  | None, None -> true
  | Some s1, Some s2 -> String.equal s1 s2
  | _, _ -> false

let rec equal_string_list sl1 sl2 =
  match sl1, sl2 with
  | [], [] -> true
  | s1 :: sl1, s2 :: sl2 ->
      String.equal s1 s2 && equal_string_list sl1 sl2
  | _, _ -> false

let rec equal_path p1 p2 =
  match p1, p2 with
  | Simple s1, Simple s2 -> String.equal s1 s2
  | Packed(sl1, s1), Packed(sl2, s2) ->
      String.equal s1 s2 && equal_string_list sl1 sl2
  | Project(p1, s1), Project(p2, s2) ->
      String.equal s1 s2 && equal_path p1 p2
  | ProjectOrSimple(p1, s1), ProjectOrSimple(p2, s2) ->
      String.equal s1 s2 && equal_path p1 p2
  | _, _ -> false

let equal t1 t2 =
  equal_path t1.path t2.path
  && equal_root t1.root t2.root

let compare_root r1 r2 =
  match r1, r2 with
  | None, None -> 0
  | None, Some _ -> 1
  | Some s1, Some s2 -> String.compare s1 s2
  | Some _, None -> -1

let rec compare_string_list sl1 sl2 =
  match sl1, sl2 with
  | [], [] -> 0
  | _ :: _, [] -> 1
  | [], _ :: _ -> -1
  | s1 :: sl1, s2 :: sl2 ->
    let c = String.compare s1 s2 in
    if c <> 0 then c
    else compare_string_list sl1 sl2

let rec compare_path p1 p2 =
  match p1, p2 with
  | Simple s1, Simple s2 -> String.compare s1 s2
  | Simple _, (Packed _ | Project _ | ProjectOrSimple _) -> 1
  | (Packed _ | Project _ | ProjectOrSimple _) , Simple _ -> -1
  | Packed(sl1, s1), Packed(sl2, s2) ->
      let c = String.compare s1 s2 in
      if c <> 0 then c
      else compare_string_list sl1 sl2
  | Packed _, (Project _ | ProjectOrSimple _) -> 1
  | (Project _ | ProjectOrSimple _) , Packed _ -> -1
  | Project(p1, s1), Project(p2, s2) ->
      let c = String.compare s1 s2 in
      if c <> 0 then c
      else compare_path p1 p2
  | Project _, ProjectOrSimple _ -> 1
  | ProjectOrSimple _, Project _ -> -1
  | ProjectOrSimple(p1, s1), ProjectOrSimple(p2, s2) ->
      let c = String.compare s1 s2 in
      if c <> 0 then c
      else compare_path p1 p2

let compare t1 t2 =
  let c = compare_path t1.path t2.path in
  if c <> 0 then c
  else compare_root t1.root t2.root

let hash = Hashtbl.hash

let print_root ppf = function
  | None -> ()
  | Some r -> Format.fprintf ppf "%s:" r

let rec print_string_list ppf = function
  | [] -> ()
  | s :: sl -> Format.fprintf ppf "%s.%a" s print_string_list sl

let rec print_path ppf = function
  | Simple s ->
      Format.fprintf ppf "%s" s
  | Packed(sl, s) ->
      Format.fprintf ppf "%a%s" print_string_list sl s
  | Project(p, s) | ProjectOrSimple(p, s) ->
      Format.fprintf ppf "%a.%s" print_path p s

let print ppf t =
  Format.fprintf ppf "%a%a"
    print_root t.root print_path t.path

let root_to_string = function
  | None -> ""
  | Some r -> r ^ ":"

let rec path_to_string = function
  | Simple s -> s
  | Packed(sl, s) ->
      String.concat "." sl ^ "." ^ s
  | Project(p, s) | ProjectOrSimple(p, s) ->
      path_to_string p ^ "." ^ s

let to_string t =
  root_to_string t.root ^ path_to_string t.path

module Ops = struct
  type nonrec t = t
  let equal = equal
  let compare = compare
  let hash = hash
end

module Set = Set.Make(Ops)

module Tbl = Hashtbl.Make(Ops)
