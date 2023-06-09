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

type 'a t =
  { effects : (string * 'a option) list }

let hash_name s =
  let accu = ref 0 in
  for i = 0 to String.length s - 1 do
    accu := 223 * !accu + Char.code s.[i]
  done;
  (* reduce to 31 bits *)
  accu := !accu land (1 lsl 31 - 1);
  (* make it signed for 64 bits architectures *)
  if !accu > 0x3FFFFFFF then !accu - (1 lsl 31) else !accu

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

let filter name effs : (_, join_error) Result.t =
  let hash = hash_name name in
  let name', tyo, effs = split_effects hash effs in
  match tyo with
  | Some _ when not (String.equal name name') ->
      Error (Different_effect_names(name, name'))
  | _ -> Ok (tyo, effs)

module General_adjustment : sig

  type 'a adjust =
    | Rename of
        { to_hash : int;
          to_index : int; }
    | Consume of 'a

  type 'a t =
    { image : string array Int.Map.t;
      adjustments : (string * 'a adjust) array Int.Map.t }

  let identity =
    let image = Int.Map.empty in
    let adjustments = Int.Map.empty in
    { image; adjustments }

  let expected_context consumed newvar t =
    let vars_tbl = Int.Tbl.create 3 in
    Int.Map.iter
      (fun hash names ->
        let vars = Array.map (fun _ -> newvar ()) names in
        Int.Tbl.add vars_tbl hash vars)
      t.image;
    let effects =
      Int.Map.fold
        (fun eff _ renames ->
          Array.fold_left
            (fun eff (name, adjustment) ->
              match adjustment with
              | Rename { to_hash; to_index } ->
                  let vars = Int.Tbl.find vars_tbl to_hash in
                  let var = vars.(to_index) in
                  (name, Some var)
              | Consume a ->
                  let tyo = consumed a in
                  (name, tyo))
           eff renames)
        [] t.renames
    in
    { effects }

  type apply_error =
    | Different_effect_names of string * string

  exception Apply_error of apply_error

  let apply consume contract eff t =
    let outer = Int.Tbl.create 3 in
    Int.Map.iter
      (fun hash names ->
        let initial = Array.map (fun name -> name, None) names in
        Int.Tbl.add outer hash initial)
      t.image;
    let eff =
      Int.Map.fold
        (fun eff hash renames ->
          Array.fold_left
            (fun eff (name, adjustment) ->
              let name', tyo, eff = split_effects hash eff in
              Option.iter
                 (fun ty ->
                   if not (String.equal name name') then begin
                     let err = Different_effect_names(from_name, name) in
                     raise (Apply_errror err)
                   end;
                   match adjustment with
                   | Rename { to_hash; to_index } ->
                       let outer = Hashtbl.find outer to_hash in
                       let outer_name, outer_tyo = outer.(to_index) in
                       let outer_ty =
                         match outer_tyo with
                         | None -> ty
                         | Some outer_ty -> contract name ty old_ty
                       in
                       outer.(to_index) <- (outer_name, Some outer_ty)
                   | Consume a ->
                       consume name ty a)
                 tyo;
              eff)
            eff renames)
        eff t.renames
    in
    let effects =
      Int.Tbl.fold
        (fun effects _ outer ->
          Array.fold_left
            (fun effects outer -> outer :: effects)
            effects outer)
        outer
    in
    { effects }

  let shift to_hash to_index t =
    match Int.Map.find_opt to_hash t.image with
    | None -> to_index
    | Some image -> to_index + Array.length image

  let apply_adjust convert mismatch t (name, adjust) image =
    let adjust =
      match adjust with
      | Consume _ -> convert adjust
      | Rename { to_hash; to_index } -> begin
          match Int.Map.find_opt to_hash t.adjustments  with
          | None ->
              let to_index = shift to_hash to_index t in
              Rename { to_hash; to_index }
          | Some adjusts ->
              let matched = Array.length adjusts in
              if to_index < matched then begin
                let image = Int.Map.find to_hash image in
                let to_name = image.(to_index) in
                let match_name, match_adjust = adjusts.(to_index) in
                if not (String.equal to_name match_name) then
                  mismatch to_name match_name
                else
                  match_adjust
              end else begin
                let to_index = shift to_hash to_index t in
                Rename { to_hash; to_index }
              end
        end
    in
    (name, adjust)

  let compose convert mismatch t1 t2 =
    let adjustments =
      Int.Map.map
        (fun _ adjusts2 -> Array.map (apply_adjust convert t1) adjusts2)
        t2.adjustments
    in
    let image = t1.image in
    let diff =
      Int.Map.merge
        (fun hash adjusts1 image2 ->
          match adjusts1, image2 with
          | None, None -> None
          | Some adjusts1, None -> Some (adjusts1, [||])
          | None, Some image2 -> Some ([||], image2)
          | Some adjusts1, Some image2 ->
              let length1 = Array.length adjusts1 in
              let length2 = Array.length image2 in
              let shared = min length1 length2 in
              let extra1 = length1 - shared in
              let extra2 = length2 - shared in
              let adjusts = Array.sub adjusts1 shared extra1 in
              let image = Array.sub image2 shared extra2 in
              adjusts, image)
        t1.adjustments t2.image
    in
    let t = { adjustments; image } in
    Int.Map.fold
      (fun hash (adjusts1, image2) t ->
        let adjusts =
          if Array.length adjusts1 = 0 then t.adjustments
          else begin
              Int.Map.update hash
                (function
                 | None -> Some adjusts1
                 | Some adjusts2 ->
                     Some (Array.append adjusts2 adjusts1))
                t.adjustments
          end
        in
        let image =
          if Array.length image2 = 0 then t.image
          else begin
              Int.Map.update hash
                (function
                 | None -> Some image2
                 | Some image1 ->
                     Some (Array.append image1 image2))
                t.image
          end
        in
        { image; adjustments })
      diff t

  let count_identities hash adjusts counts images =
    let rec loop rev_index =
      let from_index = (Array.length adjusts) - rev_index in
      let name, adjust = adjusts.(from_index) in
      match adjust with
      | Consume _ -> 0
      | Rename { to_hash; to_index } ->
          if not (Int.equal hash to_hash) then rev_index - 1
          else begin
            let to_rev_index = (Array.length images) - to_index in
            if not (Int.equal rev_index to_rev_index) then rev_index - 1
            else begin
              if not (Int.equal counts.(to_index) 1) then rev_index - 1
              else loop (rev_index + 1)
            end
          end
    in
    loop 0

  let normalize t =
    let counts = Int.Map.map (fun _ names -> Array.map (fun _ -> 0)) t.image in
    Int.Map.iter
      (fun from_hash adjusts ->
        Array.iter
          (fun (name, adjust) ->
            match adjust with
            | Consume _ -> ()
            | Rename { to_hash; to_index } ->
                let count = Int.Map.find to_hash counts in
                count.(to_index) <- count.(to_index) + 1)
          adjusts)
      t.adjustments;
    let image_ref = ref t.image in
    let adjustments =
      Int.Map.map
        (fun hash adjusts ->
          match Int.Map.find hash counts with
          | None -> adjusts
          | Some counts ->
              match Int.Map.find hash image with
              | None -> adjusts
              | Some images ->
                  let identities =
                    count_identities hash adjusts counts images
                  in
                  if Int.equal identities 0 then adjusts
                  else begin
                    let adjusts =
                      Array.sub adjusts 0 (Array.length adjusts - identities)
                    in
                    let images =
                      Array.sub images 0 (Array.length images - identities)
                    in
                    image_ref := Int.Map.add hash images !image_ref;
                    adjusts
                  end)
        t.adjustments
    in
    let image = !image_ref in
    { adjustments; image }

  let equal = failwith "TODO"

end

module Renaming = struct

  type empty = |

  type t = empty General_adjustment.t

  let identity = General_adjustment.identity

  let consume = function (_ : empty) -> .

  let apply contract ctx t =
    General_adjustment.apply contract consume

  type compose_error =
    | Different_effect_names of string * string

  exception Compose_error of compose_error

  let mismatch name1 name2 =
    raise (Compose_error(Different_effect_names(name1, name2)))

  let compose t1 t2 =
    General_adjustment.compose consume mismatch t1 t2

  let normalize t = General_adjustment.normalize t

  type parse_error =
    | Multiply_bound_variable of string loc
    | Unmatched_effect_renaming_binding of string loc
    | Unused_effect_renaming_binding of string loc

  val parse :
    outer:string loc option list
    -> inner:string loc list
    -> (t, parse_error) Result.t

end

module Adjustment = struct

  type 'a ctx = 'a t

  type 'a t = 'a option General_adjustment.t

  let identity = General_adjustment.identity

  type apply_error = General_adjustment.apply_error

  exception Apply_error = General_adjustment.Apply_error

  let apply join equal ctx t =
    General_adjustment.apply join equal ctx t

  let compose t1 t2 =
    General_adjustment.compose
      (fun x -> x) (fun _ _ -> None)
      t1 t2

  let normalize t = General_adjustment.normalize t

  let expected_context newvar t =
    expected_context (fun x -> x) newvar t

  let compose_renaming t r =
    General_adjustment.compose
      (function (_ : empty) -> .) (fun _ _ -> None)
      t1 t2

  let of_extension ctx =
    let image = Int.Map.empty in
    let rev_adjustments =
      List.fold_left
        (fun rev_adjustments (name, tyo) ->
          let hash = hash_name name in
          Int.Map.update hash
            (fun prev ->
               let prev = Option.value prev ~default:[] in
               (name, Consume tyo) :: prev)
            rev_adjustments)
        Int.Map.empty ctx
    in
    let rev_adjustments =
      Int.Map.map
        (fun rev_adjusts -> Array.of_list (List.rev rev_adjusts))
        rev_adjustments
    in
    { image; adjustments }

  let compose_extension t ctx =
    compose t (of_extension ctx)

  type equal_error =
    | Different_effect_names of string * string
    | Missing_effect of position * string

  let equal = failwith "Not implemented"

  module Desc : sig

    type t =
      { outer : string option list;
        inner : string list; }

  end

  val desc : t -> Desc.t

end
