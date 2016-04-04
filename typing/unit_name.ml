
type t = {
  root : string option;
  parents : string list;
  packs : string list;
  name : string;
}

let simple ~name =
  { root = None;
    parents = [];
    packs = [];
    name; }

let relative ~parents ~name =
  { root = None;
    parents;
    packs = [];
    name; }

let absolute ~root ~parents ~name =
  { root;
    parents;
    packs = [];
    name; }

let pack ~pack t =
  { t with packs = pack :: t.packs }

let unpacked t =
  match t.packs with
  | [] -> t
  | pack :: _ -> { t with packs = []; name = pack }

let dummy =
  { root = None;
    parents = [];
    packs = [];
    name = "" }

let equal_string_option o1 o2 =
  match o1, o2 with
  | None, None -> true
  | Some s1, Some s2 -> String.equal s1 s2
  | _, _ -> false

let rev equal_string_list l1 l2 =
  match ps1, ps2 with
  | [], [] -> true
  | s1 :: l1, s2 :: l2 ->
      String.equal s1 s2 && equal_string_list l1 l2
  | _, _ -> false

let equal t1 t2 =
  String.equal t1.name t2.name
  && equal_string_list t1.packs t2.packs
  && equal_string_list t1.parents t2.parents
  && equal_root t1 t2

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
    let c = compare_string_list t1.packs t2.packs in
    if c <> 0 then c
    else begin
      let c = compare_string_list t1.parents t2.parents in
      if c <> 0 then c
      else compare_string_option t1.root t2.root
    end
  end

let hash = Hashtbl.hash

let print_root ppf t =
  match t with
  | None -> ()
  | Some r -> Format.fprintf ppf "%s:" r

let rec print_prefixes ppf ps =
  match ps with
  | [] -> ()
  | p :: ps ->
      Format.fprintf "%a%s." print_prefixes ps p

let print ppf t =
  Format.fprintf ppf "%a%a%a%s"
    print_root t print_prefixes t.parents
    print_prefixes t.packs t.name

module Set = Set.Make(struct type nonrec t = t let compare = compare end)

module Tbl = Hashtbl.Make(struct type nonrec t = t let hash = hash end)
