(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Leo White, Jane Street                               *)
(*                                                                        *)
(*   Copyright 2023 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Misc.Stdlib

let hash_name s =
  let accu = ref 0 in
  for i = 0 to String.length s - 1 do
    accu := 223 * !accu + Char.code s.[i]
  done;
  (* reduce to 31 bits *)
  accu := !accu land (1 lsl 31 - 1);
  (* make it signed for 64 bits architectures *)
  if !accu > 0x3FFFFFFF then !accu - (1 lsl 31) else !accu

type 'ty t =
  { effects : (string * 'ty option) list }

let associate_effects t1 t2 =
  let only_in_1 name tyo =
    match tyo with
    | None -> (name, None, None)
    | Some ty -> (name, Some (name, ty), None)
  in
  let only_in_2 name tyo =
    match tyo with
    | None -> (name, None, None)
    | Some ty -> (name, None, Some (name, ty))
  in
  let in_both name1 tyo1 name2 tyo2 =
    match tyo1, tyo2 with
    | None, None -> (name1, None, None)
    | Some ty1, None -> (name1, Some (name1, ty1), None)
    | None, Some ty2 -> (name2, None, Some (name2, ty2))
    | Some ty1, Some ty2 -> (name1, Some (name1, ty1), Some (name2, ty2))
  in
  let effs1 = t1.effects in
  let effs2 = t2.effects in
  match effs1, effs2 with
  | [], [] -> []
  | _ :: _, [] -> List.map (fun (n, tyo) -> only_in_1 n tyo) effs1
  | [], _ :: _ -> List.map (fun (n, tyo) -> only_in_2 n tyo) effs2
  | _ :: _, _ :: _ ->
      let effs1 =
        List.map (fun (n, tyo) -> hash_name n, n, tyo) effs1
      in
      let effs2 =
        List.map (fun (n, tyo) -> hash_name n, n, tyo) effs2
      in
      let effs1 =
        List.stable_sort
          (fun (hash1, _, _) (hash2, _, _) -> Int.compare hash1 hash2)
          effs1
      in
      let effs2 =
        List.stable_sort
          (fun (hash1, _, _) (hash2, _, _) -> Int.compare hash1 hash2)
          effs2
      in
      let rec loop acc effs1 effs2 =
        match effs1, effs2 with
        | effs1, [] ->
            let rest = List.map (fun (_, n, tyo) -> only_in_1 n tyo) effs1 in
            List.rev_append acc rest
        | [], effs2 ->
            let rest = List.map (fun (_, n, tyo) -> only_in_2 n tyo) effs2 in
            List.rev_append acc rest
        | (hash1, name1, tyo1) :: rest1, (hash2, name2, tyo2) :: rest2 ->
            if hash1 = hash2 then
              loop ((in_both name1 tyo1 name2 tyo2) :: acc) rest1 rest2
            else if hash1 < hash2 then
              loop ((only_in_1 name1 tyo1) :: acc) rest1 effs2
            else
              loop ((only_in_2 name2 tyo2) :: acc) effs1 rest2
      in
      loop [] effs1 effs2

let split_effects hash t =
  let rec loop acc = function
    | [] -> name, None, { effects = List.rev acc }
    | (name', tyo) as eff :: rest ->
        if Int.equal (hash_name name') hash then
          name', tyo, { effects = List.rev_append acc rest }
        else
          loop (eff :: acc) rest
  in
  loop [] t.effects

type join_error =
  | Different_effect_names of string * string

exception Join_error of join_error

let join join t1 t2 =
  let pairs = associate_effects t1 t2 in
  match
    List.map
      (fun (n, eff1, eff2) ->
        match eff1, eff2 with
        | None, None -> (n, None)
        | None, Some (n2, ty2) -> (n2, Some ty2)
        | Some (n1, ty1), None -> (n1, Some ty1)
        | Some (n1, ty1), Some (n2, ty2) ->
            if not (String.equal n1 n2) then
              raise (Join_error(Different_effect_names(n1, n2)));
            let ty = join n1 ty1 ty2 in
            (n1, Some ty))
      pairs
  with
  | effects -> Ok { effects }
  | exception Join_error err -> Error err

type position =
  | First
  | Second

type equal_error =
  | Different_effect_names of string * string
  | Missing_effect of position * string

exception Equal_error of equal_error

let equal equal acc t1 t2 =
  let pairs = associate_effects t1 t2 in
  match
    List.fold_left
      (fun acc (_, eff1, eff2) ->
        match eff1, eff2 with
        | None, None -> acc
        | None, Some (missing, _) ->
            raise (Equal_error(Missing_effect (Second, missing)))
        | Some (missing, _), None ->
            raise (Equal_error(Missing_effect (First, missing)))
        | Some (n1, ty1), Some (n2, ty2) ->
            if not (String.equal n1 n2) then
              raise (Equal_error(Different_effect_names(n1, n2)));
            equal acc n1 ty1 ty2)
      pairs
  with
  | res -> Ok res
  | exception Equal_error err -> Error err

type subeffect_error =
  | Different_effect_names of string * string
  | Missing_effect of string

exception Subeffect_error of subeffect_error

let subeffect subtype t1 t2 =
  let pairs = associate_effects eff1 eff2 in
  match
    List.iter
      (fun (_, eff1, eff2) ->
        match eff1, eff2 with
        | None, (None | Some _) -> ()
        | Some (missing, _), None ->
            raise (Subeffect_error(Missing_effect missing))
        | Some (n1, ty1), Some (n2, ty2) ->
            if not (String.equal n1 n2) then
              raise (Subeffect_error (Different_effect_names(n1, n2)));
            subtype n1 t1 t2)
      pairs
  with
  | () -> Ok ()
  | exception Subeffect_error err -> Error err

type handle_error =
  | Different_effect_names of string * string

let handle name effs : (_, handle_error) Result.t =
  let hash = hash_name name in
  let name', tyo, effs = split_effects hash effs in
  match tyo with
  | Some _ when not (String.equal name name') ->
      Error (Different_effect_names(name, name'))
  | _ -> Ok (tyo, effs)

