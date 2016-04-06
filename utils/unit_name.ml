
type t = {
  root : string option;
  parents : string list;
  name : string;
}

let root { root } = root

let parents { parents } = parents

let name { name } = name

let simple ~name =
  { root = None;
    parents = [];
    name; }

let relative ~parents ~name =
  { root = None;
    parents;
    name; }

let absolute ~root ~parents ~name =
  { root = Some root;
    parents;
    name; }

let dummy =
  { root = None;
    parents = [];
    name = "" }

let equal_string_option o1 o2 =
  match o1, o2 with
  | None, None -> true
  | Some s1, Some s2 -> String.equal s1 s2
  | _, _ -> false

let rec equal_string_list l1 l2 =
  match l1, l2 with
  | [], [] -> true
  | s1 :: l1, s2 :: l2 ->
      String.equal s1 s2 && equal_string_list l1 l2
  | _, _ -> false

let equal t1 t2 =
  String.equal t1.name t2.name
  && equal_string_list t1.parents t2.parents
  && equal_string_option t1.root t2.root

let compare_string_option o1 o2 =
  match o1, o2 with
  | None, None -> 0
  | None, Some _ -> 1
  | Some s1, Some s2 -> String.compare s1 s2
  | Some _, None -> -1

let rec compare_string_list l1 l2 =
  match l1, l2 with
  | [], [] -> 0
  | [], _ :: _ -> 1
  | s1 :: l1, s2 :: l2 ->
      let c = String.compare s1 s2 in
      if c <> 0 then c
      else compare_string_list l1 l2
  | _ :: _ , [] -> -1

let compare t1 t2 =
  let c = String.compare t1.name t2.name in
  if c <> 0 then c
  else begin
    let c = compare_string_list t1.parents t2.parents in
    if c <> 0 then c
    else compare_string_option t1.root t2.root
  end

let hash = Hashtbl.hash

let print_root ppf t =
  match t.root with
  | None -> ()
  | Some r -> Format.fprintf ppf "%s:" r

let print_parents ppf t =
  let rec loop ppf = function
    | [] -> ()
    | p :: ps ->
        Format.fprintf ppf "%a%s." loop ps p
  in
    loop ppf t.parents

let print ppf t =
  Format.fprintf ppf "%a%a%s"
    print_root t print_parents t t.name

module Ops = struct
  type nonrec t = t
  let equal = equal
  let compare = compare
  let hash = hash
end

module Set = Set.Make(Ops)

module Tbl = Hashtbl.Make(Ops)
