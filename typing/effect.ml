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

type position = First | Second

module Context = struct

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
              equal acc n1 i ty1 ty2)
        acc pairs
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

end

module Adjustment = struct

  type outer = string option

  type 'ty inner =
    | Rename of string
    | Bind of 'ty option

  type functional = private Functional
  type relational = private Relational

  type ('ty, 'knd) adjust =
    | Rename :
        { to_hash : int;
          to_index : int; } -> ('ty, 'knd) adjust
    | Bind : 'ty -> ('ty, 'knd) adjust
    | Meet : ('ty, _) adjust * ('ty, _) adjust -> ('ty, relational) adjust
    | Top : ('ty, 'knd) adjust

  type ('ty, 'knd) gen =
    { image : string array Int.Map.t;
      adjustments : (string * 'ty adjust) array Int.Map.t }

  type 'ty t = ('ty, functional) gen

  let map f t =
    let adjustments =
      Int.Map.map
        (Array.map
           (fun (name, adjust) ->
             match adjust with
             | Rename _ as adjust -> adjust
             | Bind ty -> Bind (f ty)))
        adjustments
    in
    { t with adjustments }

  let get_image t hash =
    Option.default ~default:[||]
      (Int.Map.find_opt hash t.image) 

  let get_adjustments t hash =
    Option.default ~default:[||]
      (Int.Map.find_opt hash t.adjustments)

  let get_idx_adjustment image adjustments hash idx =
    let adjustments_length = Array.length adjustments in
    if idx < adjustments_length then adjustments.(idx)
    else begin
      let image_length = Array.length image in
      let to_idx = (idx - adjustments_length) + image_length in
      let to_hash = hash in
      Rename { to_hash; to_idx }
    end

  let merge f array_map1 array_map2 =
    Int.Map.merge
      (fun key array1 array2 ->
         let array1 = Option.value ~default:[||] array1 in
         let array2 = Option.value ~default:[||] array2 in
         f key array1 array2)

  let iter2 f array_map1 array_map2 =
    merge
      (fun key array1 array2 ->
        f key array1 array2;
        None)
      array_map1 array_map2

  module Parse_error = struct

    type t =
      | Multiple_outer_uses of string
      | No_outer_uses of string
      | No_inner_uses of string

    let print ppf = function
      | Multiple_outer_uses var ->
          Format.fprintf ppf
            "%s appears multiple times in the outer context"
            var
      | No_outer_uses var ->
          Format.fprintf ppf
            "%s does not appear in the outer context"
            var
      | No_inner_uses var ->
          Format.fprintf ppf
            "%s does not appear in the inner context"
            var
  end

  exception Parse_error of Parse_error.t

  let parse ~outer ~inner =
    let image, bindings =
      List.fold_left
        (fun (image, bindings) (name, var) ->
          let hash = hash_name name in
          let prev, index =
            match Int.Map.find_opt hash image with
            | None -> [], 0
            | Some (prev, length) -> prev, length
          in
          let image = Int.Map.add hash (name :: prev, index + 1) image in
          let bindings =
            match var with
            | None -> bindings
            | Some var ->
                match String.Map.find_opt var bindings with
                | Some _ -> raise (Parse_error (Multiple_outer_uses var))
                | None -> Strint.Map.add var (hash, index, ref false)
          in
          image, bindings)
        (Int.Map.empty, String.Map.empty) outer
    in
    let adjustments =
      List.fold_left
        (fun adjustments (name, (inner : _ inner)) ->
          let hash = hash_name name in
          let prev =
            match Int.Map.find_opt hash adjustments with
            | None -> []
            | Some prev -> prev
          in
          let adjust =
            match inner with
            | Bind (Some ty) -> Bind ty
            | Bind None -> Top
            | Rename var ->
                match String.Map.find_opt var bindings with
                | None -> raise (Parse_error (No_outer_uses var))
                | Some (to_hash, to_index, used) ->
                    used := true;
                    Rename { to_hash; to_index }
          in
          Int.Map.add hash name adjustments)
        Int.Map.empty inner
    in
    Int.Map.iteri
      (fun var (_, _, used) ->
        if not !used then raise (Parse_error (No_inner_uses var)))
      bindings;
    let image = Int.Map.map (fun (l, _) -> Array.of_list l) image in
    let adjustments = Int.Map.map Array.of_list adjustments in
    { image; adjustments }    

  let parse ~outer ~inner =
    match parse ~outer ~inner with
    | t -> Ok t
    | exception Parse_error err -> Error err

  let identity =
    let image = Int.Map.empty in
    let adjustments = Int.Map.empty in
    { image; adjustments }

  let handler ctx =
    let image = Int.Map.empty in
    let rev_adjustments =
      List.fold_left
        (fun rev_adjustments (name, tyo) ->
          let hash = hash_name name in
          Int.Map.update hash
            (fun prev ->
               let prev = Option.value prev ~default:[] in
               (name, Bind tyo) :: prev)
            rev_adjustments)
        Int.Map.empty ctx

    let adjustments =
      Int.Map.map
        (fun rev_adjusts -> Array.of_list (List.rev rev_adjusts))
        rev_adjustments
    in
    { image; adjustments }

  let expected_context ~newvar t =
    let vars_tbl = Int.Tbl.create 3 in
    Int.Map.iter
      (fun hash names ->
        let vars = Array.map (fun _ -> newvar ()) names in
        Int.Tbl.add vars_tbl hash vars)
      t.image;
    let effects =
      Int.Map.fold
        (fun eff _ adjustments ->
          Array.fold_left
            (fun eff (name, (adjustment : (_, functional) adjust) ->
              match adjustment with
              | Rename { to_hash; to_index } ->
                  let vars = Int.Tbl.find vars_tbl to_hash in
                  let var = vars.(to_index) in
                  (name, Some var)
              | Bind ty ->
                  (name, Some ty)
              | Top ->
                  (name, None))
           eff adjustments)
        [] t.adjustments
    in
    { effects }

  type apply_error = Effect_context.handle_error =
    | Different_effect_names of string * string

  exception Apply_error of apply_error

  let apply consume eff t =
    let outer = Int.Tbl.create 3 in
    Int.Map.iter
      (fun hash names ->
        let initial = Array.map (fun name -> name, None) names in
        Int.Tbl.add outer hash initial)
      t.image;
    let eff =
      Int.Map.fold
        (fun eff hash adjustments ->
          Array.fold_left
            (fun (eff, i) (name, (adjustment : (_, functional) adjust) ->
              let eff =
                match Effect_context.handle name eff with
                | Error err -> raise (Apply_error err)
                | Ok (None, eff) -> eff
                | Ok (Some ty, eff) -> begin
                    match adjustment with
                    | Rename { to_hash; to_index } ->
                        let outer = Hashtbl.find outer to_hash in
                        let outer_name, outer_tyo = outer.(to_index) in
                        let outer_ty =
                          match outer_tyo with
                          | None -> ty
                          | Some outer_ty -> contract name ty old_ty
                        in
                        outer.(to_index) <- (outer_name, Some outer_ty);
                        eff
                    | Bind a ->
                        consume name i ty (Some a);
                        eff
                    | Top ->
                        consume name i ty None;
                        eff
                  end
              in
              eff, i + 1)
            (eff, 0) adjustments)
        eff t.adjustments
    in
    let outer_effs =
      Int.Tbl.fold
        (fun _ outer effs ->
          Array.fold_right (fun outer effs -> outer :: effs) effects outer)
        outer []
    in
    Effect_context.append outer_eff eff

  let apply consume t1 t2 =
    match apply consume t1 t2 with
    | () -> Ok ()
    | exception Apply_error err -> Error err

  type rename_sort =
    | Active
    | Passive

  type apply_name_result =
    | Bound
    | Renamed of string * index * rename_sort

  let apply_name t name index =
    let hash = hash_name name in
    let adjusts = get_adjustments t hash in
    let matched = Array.length adjusts in
    if index < matched then begin
      match adjusts.(index) with
      | _, Bind _ -> Bound
      | _, Rename { to_hash; to_index } ->
          let images = Int.Map.find t.image to_hash in
          let to_name = images.(to_index) in
          Renamed (to_name, to_index, Active)
    end else begin
      let index = index - matched in
      let index = shift hash index t in
      Renamed (name, index, Passive)
    end

  let first_transparent t hash =
    Array.length (get_adjustments t hash)

  let equal consumed ~different_input_names ~different_output_names
        ~different_at t1 t2 =
    let _ =
      iter2
        (fun from_hash adjusts1 adjusts2 ->
          let len1 = Array.length adjusts1 in
          let len2 = Array.length adjusts2 in
          if len1 < len2 then begin
            let name, adjust = adjusts.(len1) in
            different_at name len1
          end;
          if len2 < len1 then begin
            let name, _ = adjusts.(len2) in
            different_at name len2
          end;
          for i = 0 to len1 do
            let (name1, adjust1) = adjusts1.(i) in
            let (name2, adjust2) = adjusts2.(i) in
            if not (String.equal name1 name2) then
              different_input_names name1 name2 i;
            match adjust1, adjust2 with
            | Rename _, (Bind _ | Top)| (Top | Bind _), Rename _ ->
                different_at name1 i
            | Bind c1, Bind c2 -> consumed name1 i (Some c1) (Some c2)
            | Bind c1, Top -> consumed name1 i (Some c1) None
            | Top, Bind c2 -> consumed name1 i None (Some c2)
            | Top, Top -> consumed name1 i None None
            | Rename { to_hash = to_hash1;
                       to_index = to_index1; },
              Rename { to_hash = to_hash2;
                       to_index = to_index2; } ->
                if (not (Int.equal to_hash1 to_hash2))
                   || (not (Int.equal to_index1 to_index2)) then
                  different_at name1 i;
                let to_image1 = Int.Map.find t1.image to_hash1 in
                let to_image2 = Int.Map.find t2.image to_hash2 in
                let to_name1 = to_image1.(to_index1) in
                let to_name2 = to_image2.(to_index2) in
                if not (String.equal to_name1 to_name2) then begin
                  different_output_names
                    ~origin:name1 ~origin_index:i
                    ~first_destination:to_name1
                    ~second_destination:to_name2
                    ~destination_index:to_index
                end
          done)
        t1.adjustments t2.adjustments
    in
    let _ =
      iter2
        (fun to_hash images1 images2 ->
          let len1 = Array.length images1 in
          let len2 = Array.length images2 in
          if len1 < len2 then begin
            let name = images2.(len1) in
            let index = first_transparent t1 to_hash in
            different_at name index
          end;
          if len2 < len1 then begin
            let name = images1.(len2) in
            let index = first_transparent t2 to_hash in
            different_at name index
          end)
        t1.image t2.image
    in
    ()

  module Simplified = struct

    type 'ty gen = 'ty gen

    type 'ty t = 'ty option gen

    let is_identity t =
      Int.Map.is_empty t.image
      && Int.Map.is_empty t.adjustments

    let rec is_transparent : type k. _ -> _ -> (_, k) adjust -> _ -> _ =
      fun hash from_rev_index adjust images ->
        match adjust with
        | Bind _ | Top -> false
        | Rename { to_hash; to_index } ->
            if not (Int.equal hash to_hash) then false
            else begin
              let to_rev_index = (Array.length images - 1) - to_index in
              Int.equal from_rev_index to_rev_index
            end
        | Meet(adjust1, adjust2) ->
            is_transparent hash from_rev_index adjust1 images
            && is_transparent hash from_rev_index adjust2 images

    let count_transparent hash adjusts images =
      let rec loop from_rev_index =
        let from_index = (Array.length adjusts - 1) - from_rev_index in
        let _, adjust = adjusts.(from_index) in
        if is_transparent hash from_rev_index adjust images then loop (from_rev_index + 1)
        else from_rev_index
      in
      loop 0

    let normalize t =
      let image_ref = ref t.image in
      let adjustments =
        Int.Map.filter_map
          (fun hash adjusts ->
             match Int.Map.find hash image with
             | None -> Some adjusts
             | Some images ->
                 let transparent = count_transparent hash adjusts images in
                 if Int.equal transparent 0 then Some adjusts
                 else begin
                   let adjusts =
                     Array.sub adjusts 0 (Array.length adjusts - transparent)
                   in
                   let images =
                     Array.sub images 0 (Array.length images - transparent)
                   in 
                   let new_image =
                     if Array.length images > 0 then Int.Map.add hash images !image_ref
                     else Int.Map.remove hash !image_ref
                   in
                   image_ref := new_image;
                   if Array.length adjusts > 0 then Some adjusts
                   else None
                 end)
          t.adjustments
      in
      let image = !image_ref in
      { adjustments; image }

    type equal_error =
      | Different_effect_names of string * string
      | Missing_bind of string * int * position
      | Different_renames of
          { origin : string;
            origin_index : int;
            first_destination : string;
            first_destination_index : int;
            second_destination : string;
            second_destination_index : int; }

    exception Equal_error of equal_error

    let different_input_names _ _ _ = ()

    let different_output_names
          ~origin:_ ~origin_index:_ ~first_destination:_
          ~second_destination:_ ~destination_index:_ = ()

    let different_at t1 t2 origin origin_index =
      let res1 = apply_name t1 origin origin_index in
      let res2 = apply_name t2 origin origin_index in
      match res1, res2 with
      | Bound, Bound -> assert false
      | Renamed _, Bound ->
          raise Equal_error(Missing_bind(origin, origin_index, Second))
      | Bound, Renamed _ ->
          raise Equal_error(Missing_bind(origin, origin_index, First))
      | Renamed (first_destination, first_destination_index, _),
        Renamed (second_destination, second_destination_index, _) ->
          assert ((not (String.equal first_destination second_destination))
                  || (not (Int.equal first_destination_index
                             second_destination_index)));
          let err =
            Different_renames
              { origin; origin_index;
                first_destination; first_destination_index;
                second_destination; second_destination_index }
          in
          raise (Equal_error err)

    let equal consumed t1 t2 =
      match equal consumed ~different_input_names
              ~different_output_names ~different_at t1 t2 with
      | () -> Ok ()
      | exception Equal_error err -> Error err

    let shift to_hash to_index t =
      Array.length (get_image t to_hash) + to_index

    let apply_adjust t (name, adjust) =
      let adjust =
        match adjust with
        | Bind _ -> adjust
        | Rename { to_hash; to_index } -> begin
            let adjustments = get_adjustments t to_hash in
            let matched = Array.length adjustments in
            if to_index < matched then begin
              let _, match_adjust = adjustments.(to_index) in
              match_adjust
            end else begin
              let to_index = to_index - matched in
              let to_index = shift to_hash to_index t in
              Rename { to_hash; to_index }
            end
          end
      in
      (name, adjust)

    let compose t1 t2 =
      if is_identity t1 then t2
      else if is_identity t2 then t1
      else begin
        let adjustments =
          Int.Map.map
            (fun _ adjusts2 -> Array.map (apply_adjust t1) adjusts2)
            t2.adjustments
        in
        let image = t1.image in
        let diff =
          merge
            (fun hash adjusts1 image2 ->
              let length1 = Array.length adjusts1 in
              let length2 = Array.length image2 in
              let shared = min length1 length2 in
              if shared = 0 then adjusts1, image2
              else begin
                let extra1 = length1 - shared in
                let extra2 = length2 - shared in
                let adjusts = Array.sub adjusts1 shared extra1 in
                let image = Array.sub image2 shared extra2 in
                adjusts, image
              end)
            t1.adjustments t2.image
        in
        let t = { adjustments; image } in
        let t =
          Int.Map.fold
            (fun hash (adjusts1, image2) t ->
              let adjustments =
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
        in
        normalize t
      end

    module Residual_error : sig

      type t =
        | Unexpected_bind of string * int
        | Unexpected_contraction of
            { first_origin_name : string;
              first_origin_index : int;
              second_origin_name : string;
              second_origin_index : int; }

    end

    exception Residual_error of Residual_error.t

    let meet_adjustment ~meet ~check_top
          name1 idx1 adjustment1 name2 idx2 adjustment2 =
      match adjustment1, adjustment2 with
      | Bind ty1, Bind ty2 ->
          let ty =
            meet prev_name prev_index prev_ty
              name2 idx ty1
          in
          Bind ty
      | Rename _, Bind ty2 ->
          check_top name2 idx2 ty2 name1 idx1;
          adjustment1
      | Bind ty1, Rename _ ->
          check_top name1 idx1 ty1 name2 idx2;
          adjustment2
      | Rename
          { to_hash = to_hash1;
            to_index = to_index1 },
        Rename
          { to_hash = to_hash2;
            to_index = to_index2 } ->
          if Int.equal to_hash1 to_hash2
             && Int.equal to_index1 to_index2 then adjustment1
          else begin
            raise Residual_error
              (Unexpected_contraction
                 { first_origin_name = name1;
                   first_origin_index = idx1;
                   second_origin_name = name2;
                   second_origin_index = idx2; })
          end

    let left_residual_gen ~subtype ~meet ~top ~check_top t1 t2 =
      if is_identity t2 then t1
      else begin
        let image1 = ref t1.image in
        let image2 = ref t2.image in
        let union =
          merge
            (fun hash adjusts1 adjusts2 ->
               let length1 = Array.length adjusts1 in               
               let length2 = Array.length adjusts2 in
               let length = max length1 length2 in
               let img1 = get_image t1 hash in
               let img2 = get_image t2 hash in
               if length > length1 then begin
                   let extra = Array.sub adjusts2 length1 (length - length1) in
                   let extra = Array.map (fun (name, _) -> name) in
                   let new_img1 = Array.append img1 extra in
                   image1 := Int.Map.add hash new_img1;
               end;
               if length > length2 then begin
                   let extra = Array.sub adjusts1 length2 (length - length2) in
                   let extra = Array.map (fun (name, _) -> name) in
                   let new_img2 = Array.append img2 extra in
                   image2 := Int.Map.add hash new_img2;
               end;
               Array.init length
                 (fun idx ->
                   get_idx_adjustment img1 adjusts1 hash idx,
                   get_idx_adjustment img2 adjusts2 hash idx))
            t1.adjustments t2.adjustments
        in
        let image = !image1 in
        let adjustments =
          Int.Map.map
            (fun hash images ->
              Array.map (fun name -> (name, None)) images)
            !image2
        in
        Int.Map.iter
          (fun hash (shared, _, _) ->
            Array.iteri
              (fun idx ((name1, adjustment1), (name2, adjustment2)) ->
                match adjustment2 with
                | Bind ty2 -> begin
                    match adjustment1 with
                    | Rename _ -> raise (Residual_error (Unexpected_bind(name2, idx)))
                    | Bind ty1 -> subtype name2 idx ty2 ty1
                  end
                | Rename { to_hash; to_index } ->
                    let adjustments = Int.Map.find to_hash adjustments in
                    let previous = adjustments.(to_index) in
                    let adjustment =
                      match previous with
                      | None -> adjustment1
                      | Some(prev_name, prev_index, prev_adjustment) ->
                          meet_adjustment ~meet ~check_top
                            prev_name prev_index prev_adjustment
                            name2 idx adjustment1
                    in
                    adjustments.(to_index) <- Some (name2, idx, adjustment))
              t shared)
          union;
        let adjustments =
          Int.Map.map
            (fun adjustments ->
              Array.map
                (fun (name, adjustment) ->
                  let adjustment =
                    match adjustment with
                    | None -> Bind top
                    | Some(_, _, adjustment) -> adjustment
                  in
                  (name, adjustment))
                adjustments)
            adjustments
        in
        { image; adjustments }
      end

    let left_residual ~equal ~meet t1 t2 =
      let subtype name idx ty1 ty2 =
        match ty1, ty2 with
        | (None | Some _), None -> ()
        | None, Some _ ->
            raise Residual_error (Unexpected_bind(name, idx))
        | Some ty1, Some ty2 -> equal name idx ty1 ty2
      in
      let meet name1 idx1 ty1 name2 idx2 ty2 =
        match ty1, ty2 with
        | None, None -> None
        | None, Some _ -> ty2
        | Some _, None -> ty1
        | Some ty1, Some ty2 -> meet name1 idx1 ty1 name2 idx2 ty2
      in
      let top = None in
      let check_top name1 idx1 ty1 name2 idx2 =
        match ty1 with
        | None -> ()
        | Some _ ->
            raise Residual_error
              (Unexpected_contraction
                 { first_origin_name = name1;
                   first_origin_index = idx1;
                   second_origin_name = name2;
                   second_origin_index = idx2; })
      in
      match
        left_residual_gen ~subtype ~meet ~top ~check_top t1 t2
      with
      | t -> Ok t
      | exception Residual_error err -> Error err

    let reify t = t

  end

  type equal_error =
    | Different_input_names of string * string * index
    | Missing_bind of string * int * position
    | Missing_rename of string * int * position
    | Different_renames of
        { origin : string;
          origin_index : int;
          first_destination : string;
          first_destination_index : int;
          second_destination : string;
          second_destination_index : int; }

  exception Equal_error of equal_error

  let different_input_names name1 name2 index =
    raise Equal_error(Different_input_names(name1, name2, index))

  let different_output_names
        ~origin ~origin_index ~first_destination
        ~second_destination ~destination_index =
    let first_destination_index = destination_index in
    let second_destination_index = destination_index in
    let err =
      Different_renames
        { orgin; origin_index;
          first_destination; first_destination_index;
          second_destination; second_destination_index }
    in
    raise (Equal_error err)

  let different_at t1 t2 origin origin_index =
    let res1 = apply_name t1 origin origin_index in
    let res2 = apply_name t2 origin origin_index in
    match res1, res2 with
    | Bound, Bound -> assert false
    | Renamed _, Bound ->
        raise Equal_error(Missing_bind(origin, origin_index, First))
    | Bound, Renamed _ ->
        raise Equal_error(Missing_bind(origin, origin_index, Second))
    | Renamed(_, _, Passive), Renamed(_, _, Active) ->
        raise Equal_error(Missing_rename(origin, origin_index, First))
    | Renamed(_, _, Active), Renamed(_, _, Passive) ->
        raise Equal_error(Missing_rename(origin, origin_index, Second))
    | Renamed (first_destination, first_destination_index, _),
      Renamed (second_destination, second_destination_index, _) ->
        assert ((not (String.equal first_destination second_destination))
                || (not (Int.equal first_destination_index
                           second_destination_index)));
        let err =
          Different_renames
            { origin; origin_index;
              first_destination; first_destination_index;
              second_destination; second_destination_index }
        in
        raise (Equal_error err)

  let equal consumed t1 t2 =
    match equal consumed ~different_input_names
            ~different_output_names ~different_at t1 t2 with
    | () -> Ok ()
    | exception Equal_error err -> Error err

  let simplify t = Simplified.normalize t

end

module Mode = struct

(* 
   Modes are expressions from the grammar:

   {v
     M ::= adjustment A | escape | unused | unadjusted L
         | M & M | M / A | M . A | F \ M | A . M

     F ::= var X | adj A
   v}

   where A is an adjustment, X is a mode variable for a
   closure, and L is a locality.

   Note that [A . M] is a partial operation that is only defined when
   [M] is equal to one of the form [S & (adj A \ M')] for the same A, where
   S is a mode without any right residuals:

   {v
       S ::= adjustment A | escape | unused | unadjusted L
           | S & S | S / A | S . A
   v}

   Similarly, [M1 & M2] is only defined if there is some [F] for
   which [M1] is equal to one of the form [S1 & (F \ M1')] and
   [M2] is equal to one of the form [S2 & (F \ M2')] and the same
   property holds recursively between [M1'] and [M2'].

   We simplify these terms to ones of the form:

     N ::= escape | unused | O

     O ::= B | B & (F \ O) | F \ O

     B ::= adjustment A | unadjusted L | B & B | B . A | B / A

   and represent [B] using [Adjustment.Simplified.gen]. This limits the
   size of our terms to the nesting depth of closures and rewritings,
   which is expected to be small.

   This simplification relies on the following equations holding:

     1. (F \ M1) & (F \ M2) = F \ (M1 & M2)
     2. (M1 & M2) / A = (M1 / A) & (M2 / A)
     3. (F \ M) / A = F \ (M / A)
     4. (M1 & M2) . A = (M1 . A) & (M2 . A)
     5. (F \ M) . A = F \ (M . A)
     6. A . (M1 & M2) = (adj A . M1) & (adj A . M1)
     7. A1 . (M / A2) = (adj A1 . M) / A2

   Equations 1, 2 and 3 hold in any residuated lattice
   
*)


  type ('ty, 'lcl) desc =
    | Unadjusted of 'lcl
    | Adjusted of 'ty list Adjustment.Simplified.gen
    | Escaping (* equivalent to Unadjusted global *)

  type ('ty, 'lcl) constraint_ =
    | Global of 'lcl
    | Meet of string * int * 'ty * 'ty
    | Left_residual

  type ('ty, 'lcl) factor_var =
    { mutable desc : ('ty, 'lcl) desc;
      mutable global_constraints : 'lcl list;
      mutable consumed : bool; }

  type 'ty factor_adjustment =
    { mutable adjustment : 'ty Adjustment.Simplified.t }

  type ('ty, 'lcl) factor =
    | Var of ('ty, 'lcl) factor_var
    | Adjustment of 'ty factor_adjustment

  type reason =
    | Meet_error of Adjustment.Meet_error.t
    | Residual_error of Adjustment.Residual_error.t

  type ('ty, 'lcl) t =
    | Used of
        { desc : ('ty, 'lcl) desc;
          global_constraints : 'lcl list;
          factors : ('ty, 'lcl) factor list; }
    | Unused of reason list

  let used desc =
    let constraints = [] in
    Used { desc; constraints; }

  let escaping = used Escaping

  let unused = Unused []

  let adjusted adj =
    let adj = Adjustment.map Option.to_list adj in
    let adj = Adjustment.simplified adj in
    used (Adjusted adj)

  let local = used (Adjusted Adjustment.Simplified.identity)

  let unadjusted lcl = used (Unadjusted lcl)

  let meet_desc constraints desc1 desc2 =
    match desc1, desc2 with
    | Escape, desc | desc, Escape ->
        Ok (desc, constraints)
    | Unadjusted lcl1, Unadjusted lcl2 ->
        let desc = Unadjustment (join lcl1 lcl2) in
        Ok (desc, constraints)
    | Unadjusted lcl, Adjusted adj | Adjusted adj, Unadjusted lcl ->
        if Adjustment.Simplified.is_identity adj then begin
          let desc = Adjusted adj in
            Ok (desc, constraints)
        end else begin
          let desc = Adjusted adj in
            let constraints = Global lcl :: constraints in
            Ok (desc, constraints)
        end
    | Adjusted adj1, Adjusted adj2 ->
        let meet constraints name idx tys1 tys2 =
          match tys1, tys2 with
          | None, tys | tys, None -> tys
          | Some tys1, Some tys2 -> Some (tys @
        in
        match Adjustment.Simplified.meet_gen ~meet constraints adj1 adj2 with
        | Ok(adj, constraints) ->
            let desc = Adjusted adj in
            Ok (desc, constraints)
        | Error err -> Error err

  let meet ~join t1 t2 =
    match t1, t2 with
    | Unused reason, _ | _, Unused reason -> Unused reason
    | Used u1, Used u2 ->
        let constraints = u1.constraints @ u2.constraints in
        match meet_desc u1.desc u2.desc with
        | Ok (desc, constraints) -> Used { desc; constraints }
        | Error err -> Unused (Meet_error err)

  let left_residual_desc constraints desc1 desc2 =
    

  val left_residual : ('ty1, 'lcl) t -> 'ty2 Adjustment.t -> ('ty1, 'lcl) t
  (** We can take left residuals with respect to adjustments. Unlike for adjustments
      this operation is total due to the existence of [unused].  *)

  val left_residual_not_unused :
    equal:(string -> int -> 'ty1 -> 'ty2 -> unit)
    -> meet:(string -> int -> 'ty1 -> string -> int -> 'ty1 -> 'ty1)
    -> ('ty1, 'lcl) t
    -> 'ty2 Adjustment.t
    -> (('ty1, 'lcl) t, Residual_error.t) Result.t
  (** This is the same as [left_residual], unless the resulting mode
      would be [unused], in which case it gives an error describing why
      that is the case. Raises if the original mode is already
      [unused]. *)

  module Factor = struct

    type ('ty, 'lcl) t

    module Var : sig

      (* Reference holds the current upper bound *)
      type ('ty, 'lcl) t = ('ty, 'lcl) var

      let create =
        { desc = Escaping;
          constraints = [];
          consumed = false; }

      let consume t =
        t.consumed <- true;
        Used { desc = t.desc; constraints = t.constraints }

    end

    module Adjustment : sig

      type ('ty, 'lcl) t

      val create : 'ty Adjustment.Simplified.t -> ('ty, 'lcl) t

    end

    val var : ('ty, 'lcl) Var.t -> ('ty, 'lcl) t

    val adjustment : ('ty, 'lcl) Adjustment.t -> ('ty, 'lcl) t

  end

  val compose_right : ('ty, 'lcl) t -> 'ty Adjustment.t -> ('ty, 'lcl) t
  (** We can compose with adjustments on the right. *)
 
  val right_residual : ('ty, 'lcl) Factor.t -> ('ty, 'lcl) t -> ('ty, 'lcl) t

  val compose_left : ('ty, 'lcl) Factor.t -> ('ty, 'lcl) t -> ('ty, 'lcl) t

  module Submode_error : sig

    type t =
      | Escaping 
      | Adjusted of Adjustment.Equal_error.t

    val swap_position : t -> t

  end

  val submode :
    equal:(string -> int -> 'ty1 -> 'ty2 -> unit)
    -> sublocal:('lcl1 -> 'lcl2 -> unit)
    -> escaping:'lcl2
    -> ('ty1, 'lcl1) t
    -> 'ty2 Adjustment.t
    -> (unit, Submode_error.t) Result.t

  val iter : ('ty -> unit) -> 'ty t -> unit

  val fold : ('a -> 'ty -> 'a) -> 'a -> 'ty t -> 'a

  val copy : ('ty -> 'ty) -> 'ty t -> 'ty t

  val copy_fold :
    ('a -> 'ty -> 'a * 'ty) -> 'a -> 'ty t -> 'a * 'ty t

end
