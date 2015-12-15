(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

module M = struct
  type t = {
    compilation_unit : Compilation_unit.t;
    name : string;
    name_stamp : int;
    (** [name_stamp]s are unique within any given compilation unit. *)
  }

  let compare t1 t2 =
    if t1 == t2 then 0
    else
      let c = t1.name_stamp - t2.name_stamp in
      if c <> 0 then c
      else Compilation_unit.compare t1.compilation_unit t2.compilation_unit

  let equal t1 t2 =
    if t1 == t2 then true
    else
      t1.name_stamp = t2.name_stamp
        && Compilation_unit.equal t1.compilation_unit t2.compilation_unit

  let output chan t =
    output_string chan t.name;
    output_string chan "_";
    output_string chan (string_of_int t.name_stamp)

  let hash t = Hashtbl.hash t

  let print ppf t =
    if Compilation_unit.equal t.compilation_unit
        (Compilation_unit.get_current_exn ())
    then begin
      Format.fprintf ppf "%s/%d"
        t.name t.name_stamp
    end else begin
      Format.fprintf ppf "%a.%s/%d"
        Compilation_unit.print t.compilation_unit
        t.name t.name_stamp
    end
end

include M

module Set = struct

  type elt = t

  type t = elt Varset.t Compilation_unit.Map.t

  let empty = Compilation_unit.Map.empty

  let is_empty = Compilation_unit.Map.is_empty

  let mem v s =
    match Compilation_unit.Map.find v.compilation_unit s with
    | vset -> Varset.mem v.name_stamp vset
    | exception Not_found -> false

  let add v s =
    let previous =
      match Compilation_unit.Map.find v.compilation_unit s with
      | vset -> vset
      | exception Not_found -> Varset.empty
    in
    let next = Varset.add v.name_stamp v previous in
    Compilation_unit.Map.add v.compilation_unit next s

  let singleton v =
    let vset = Varset.singleton v.name_stamp v in
    Compilation_unit.Map.singleton v.compilation_unit vset

  let remove v s =
    match Compilation_unit.Map.find v.compilation_unit s with
    | previous ->
      let next = Varset.remove v.name_stamp previous in
      if Varset.is_empty next then
        Compilation_unit.Map.remove v.compilation_unit s
      else
        Compilation_unit.Map.add v.compilation_unit next s
    | exception Not_found -> s

  let union s0 s1 =
    Compilation_unit.Map.union
      (fun _ vset1 vset2 -> Varset.union vset1 vset2)
      s0 s1

  let inter s0 s1 =
    Compilation_unit.Map.merge
      (fun _ vset1 vset2 ->
         match vset1, vset2 with
         | None, None -> None
         | None, Some _ -> None
         | Some _, None -> None
         | Some vset1, Some vset2 ->
           let vset = Varset.inter vset1 vset2 in
           if Varset.is_empty vset then None
           else Some vset)
      s0 s1

  let diff s0 s1 =
    Compilation_unit.Map.merge
      (fun _ vset1 vset2 ->
         match vset1, vset2 with
         | None, None -> None
         | None, Some _ -> None
         | Some _, None -> vset1
         | Some vset1, Some vset2 ->
           let vset = Varset.diff vset1 vset2 in
           if Varset.is_empty vset then None
           else Some vset)
      s0 s1

  let compare s0 s1 =
    Compilation_unit.Map.compare Varset.compare s0 s1

  let equal s0 s1 =
    Compilation_unit.Map.equal Varset.equal s0 s1

  let subset s0 s1 =
    let conflicts =
      Compilation_unit.Map.merge
        (fun _ vset1 vset2 ->
           match vset1, vset2 with
           | None, _ -> None
           | Some _, None -> Some ()
           | Some vset1, Some vset2 ->
               if Varset.subset vset1 vset2 then None
               else Some ())
        s0 s1
    in
    Compilation_unit.Map.is_empty conflicts

  let iter f s =
    Compilation_unit.Map.iter
      (fun _ vset -> Varset.iter f vset)
      s

  let fold f s acc =
    Compilation_unit.Map.fold
      (fun _ vset acc -> Varset.fold f vset acc)
      s acc

  let for_all p s =
    Compilation_unit.Map.for_all
      (fun _ vset -> Varset.for_all p vset)
      s

  let exists p s =
    Compilation_unit.Map.exists
      (fun _ vset -> Varset.exists p vset)
      s

  let filter p s =
    Compilation_unit.Map.map
      (fun vset -> Varset.filter p vset)
      s

  let partition p s =
    Compilation_unit.Map.fold
      (fun unit vset (true_, false_) ->
         let tset, fset = Varset.partition p vset in
         let true_ = Compilation_unit.Map.add unit tset true_ in
         let false_ = Compilation_unit.Map.add unit fset false_ in
         (true_, false_))
      s (empty, empty)

  let cardinal s =
    Compilation_unit.Map.fold
      (fun _ vset acc ->
         acc + (Varset.cardinal vset))
      s 0

  let elements s =
    Compilation_unit.Map.fold
      (fun _ vset acc -> List.rev_append (Varset.elements vset) acc)
      s []

  let min_elt s =
    let _, vset = Compilation_unit.Map.min_binding s in
    Varset.min_elt vset

  let max_elt s =
    let _, vset = Compilation_unit.Map.max_binding s in
    Varset.max_elt vset

  let choose s =
    let _, vset = Compilation_unit.Map.choose s in
    Varset.choose vset

  let split v s =
    let lt, vset, gt = Compilation_unit.Map.split v.compilation_unit s in
    match vset with
    | None -> lt, false, gt
    | Some vset ->
      let lset, mem, gset = Varset.split v.name_stamp vset in
      let lt = Compilation_unit.Map.add v.compilation_unit lset lt in
      let gt = Compilation_unit.Map.add v.compilation_unit gset gt in
      lt, mem, gt

  let find v s =
    let vset = Compilation_unit.Map.find v.compilation_unit s in
      Varset.find v.name_stamp vset

  let of_list = function
    | [] -> empty
    | [t] -> singleton t
    | t :: q -> List.fold_left (fun acc e -> add e acc) (singleton t) q

  let output oc s =
    Printf.fprintf oc "( ";
    iter (fun v -> Printf.fprintf oc "%a " M.output v) s;
    Printf.fprintf oc ")"

  let print ppf s =
    let elts ppf s =
      iter (fun e -> Format.fprintf ppf "@ %a" M.print e) s
    in
    Format.fprintf ppf "@[<1>{@[%a@ @]}@]" elts s

  let to_string s = Format.asprintf "%a" print s

  let map f s = of_list (List.map f (elements s))

end

module Map = struct
  include Ext_types.ExtMap(M)(Set)

  let keys map = fold (fun k _ set -> Set.add k set) map Set.empty

  let of_set f set = Set.fold (fun e map -> add e (f e) map) set empty

end

module Tbl = Ext_types.ExtHashtbl(M)(Set)(Map)

let previous_name_stamp = ref (-1)

let create ?current_compilation_unit name =
  let compilation_unit =
    match current_compilation_unit with
    | Some compilation_unit -> compilation_unit
    | None -> Compilation_unit.get_current_exn ()
  in
  let name_stamp =
    incr previous_name_stamp;
    !previous_name_stamp
  in
  { compilation_unit;
    name;
    name_stamp;
  }

let create_with_same_name_as_ident ident = create (Ident.name ident)

let clambda_name t =
  Format.asprintf "%a_%s"
    Compilation_unit.print t.compilation_unit
    t.name

let rename ?current_compilation_unit ?append t =
  let current_compilation_unit =
    match current_compilation_unit with
    | Some compilation_unit -> compilation_unit
    | None -> Compilation_unit.get_current_exn ()
  in
  let name =
    match append with
    | None -> t.name
    | Some s -> t.name ^ s
  in
  create ~current_compilation_unit name

let in_compilation_unit t cu =
  Compilation_unit.equal cu t.compilation_unit

let get_compilation_unit t = t.compilation_unit

let unique_name t =
  Printf.sprintf "%s_%d" t.name t.name_stamp

let print_list ppf ts =
  List.iter (fun t -> Format.fprintf ppf "@ %a" print t) ts

let debug_when_stamp_matches t ~stamp ~f =
  if t.name_stamp = stamp then f ()

let print_opt ppf = function
  | None -> Format.fprintf ppf "<no var>"
  | Some t -> print ppf t

type pair = t * t
module Pair = Ext_types.Identifiable.Make (Ext_types.Pair (M) (M))

let compare_lists l1 l2 = Misc.compare_lists compare l1 l2

let output_full chan t =
  Compilation_unit.output chan t.compilation_unit;
  output_string chan ".";
  output chan t
