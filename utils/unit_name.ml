
type path =
  | Simple of string
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

let prefix ~prefix ~uname =
  match uname.root with
  | Some _ -> uname
  | None ->
      let root = prefix.root in
      let rec loop = function
        | Simple name -> ProjectOrSimple(prefix.path, name)
        | Project(parent, name) -> Project(loop parent, name)
        | ProjectOrSimple(parent, name) -> ProjectOrSimple(loop parent, name)
      in
      let path = loop uname.path in
      { root; path }

let equal_root r1 r2 =
  match r1, r2 with
  | None, None -> true
  | Some s1, Some s2 -> String.equal s1 s2
  | _, _ -> false

let rec equal_path p1 p2 =
  match p1, p2 with
  | Simple s1, Simple s2 -> String.equal s1 s2
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

let rec compare_path p1 p2 =
  match p1, p2 with
  | Simple s1, Simple s2 -> String.compare s1 s2
  | Simple _, (Project _ | ProjectOrSimple _) -> 1
  | (Project _ | ProjectOrSimple _) , Simple _ -> -1
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

let rec print_path ppf = function
  | Simple s ->
      Format.fprintf ppf "%s" s
  | Project(p, s) | ProjectOrSimple(p, s) ->
      Format.fprintf ppf "%a.%s" print_path p s

let print ppf t =
  Format.fprintf ppf "%a%a"
    print_root t.root print_path t.path

module Ops = struct
  type nonrec t = t
  let equal = equal
  let compare = compare
  let hash = hash
end

module Set = Set.Make(Ops)

module Tbl = Hashtbl.Make(Ops)
