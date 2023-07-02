type empty = |

type position = First | Second

module General_adjustment = struct

  type 'ty adjust =
    | Rename of
        { to_hash : int;
          to_index : int; }
    | Bind of 'ty

  type 'ty t =
    { image : string array Int.Map.t;
      adjustments : (string * 'ty adjust) array Int.Map.t }

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
              | Bind a ->
                  let tyo = consumed a in
                  (name, tyo))
           eff renames)
        [] t.renames
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
        (fun eff hash renames ->
          Array.fold_left
            (fun (eff, i) (name, adjustment) ->
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
                        consume name i ty a;
                        eff
                  end
              in
              eff, i + 1)
            (eff, 0) renames)
        eff t.renames
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
    let adjusts =
      match Int.Map.find_opt hash t.adjustments  with
      | None -> [||]
      | Some adjusts -> adjusts
    in
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
    match Int.Map.find_opt hash t.adjustments with
    | None -> 0
    | Some adjusts -> Array.length adjusts

  let equal consumed ~different_input_names ~different_output_names
        ~different_at t1 t2 =
    let _ =
      Int.Map.merge
        (fun from_hash adjusts1 adjusts2 ->
          let adjusts1 = Option.value ~default:[||] adjusts1 in
          let adjusts2 = Option.value ~default:[||] adjusts2 in
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
            | Rename _, Bind _ | Bind _, Rename _ ->
                different_at name1 i
            | Bind c1, Bind c2 -> consumed name1 i c1 c2
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
          done;
          None)
        t1.adjustments t2.adjustments
    in
    let _ =
      Int.Map.merge
        (fun to_hash images1 images2 ->
          let images1 = Option.value ~default:[||] images1 in
          let images2 = Option.value ~default:[||] images2 in
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
          end;
          None)
        t1.image t2.image
    in
    ()

  module Simplified = struct

    type 'ty t = 'ty t

    let is_identity t =
      Int.Map.is_empty t.image
      && Int.Map.is_empty t.adjustments

    let is_transparent hash from_rev_index adjust images =
      match adjust with
      | Bind _ -> false
      | Rename { to_hash; to_index } ->
          if not (Int.equal hash to_hash) then false
          else begin
            let to_rev_index = (Array.length images - 1) - to_index in
            Int.equal from_rev_index to_rev_index
          end

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

    let shift to_hash to_index t =
      match Int.Map.find_opt to_hash t.image with
      | None -> to_index
      | Some image -> to_index + Array.length image

    let apply_adjust t (name, adjust) image =
      let adjust =
        match adjust with
        | Bind _ -> adjust
        | Rename { to_hash; to_index } -> begin
            match Int.Map.find_opt to_hash t.adjustments  with
            | None ->
                let to_index = shift to_hash to_index t in
                Rename { to_hash; to_index }
            | Some adjusts ->
                let matched = Array.length adjusts in
                if to_index < matched then begin
                  let _, match_adjust = adjusts.(to_index) in
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

end

module Renaming = struct

  type t = empty General_adjustment.t

  let identity = General_adjustment.identity

  let consume = function (_ : empty) -> .

  let apply ctx t =
    General_adjustment.apply consume

  let compose t1 t2 =
    General_adjustment.composet1 t2

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

  type 'ty t = 'ty option General_adjustment.t

  let identity = General_adjustment.identity

  type apply_error = General_adjustment.apply_error

  exception Apply_error = General_adjustment.Apply_error

  let apply equal ctx t =
    General_adjustment.apply equal ctx t

  let compose t1 t2 = General_adjustment.compose t1 t2

  let sequence t1 t2 = compose t2 t1

  let expected_context newvar t =
    expected_context (fun x -> x) newvar t

  (* Need magic to do this efficiently due to a lack of
     immutable arrays and no subtyping on the empty type. *)
  let of_renaming (r : empty General_adjustment.t) =
    (Obj.magic r : 'ty t)

  let of_extension ctx =
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

  let equal consumed t1 t2 =
    General_adjustment.equal consumed t1 t2

end

module Mode = struct

  type 'ty t =
    | Md_global
    | Md_unused
    | Md_local
    | Md_adjustment of 'ty Adjustment.t
    | Md_var of 'ty var

  and 'ty var =
    { mutable state : 'ty state }

  and 'ty state =
    | Pressured of 'ty const
    | Relaxed of { mutable constraints : ('ty t list * Location.t) list }
    | Escaped of Location.t

  and 'ty const =
    | Global of Location.t
    | Used of ('ty Adjustment.t * Location.t) list

  let var v = Var v

  let global = Global

  let unused = Unused

  let adjustment adj =
    if Adjustment.is_identity adj then Local
    else Adjusement adj

  let extension ext =
    adjustment (Adjustment.of_extension ext)

  let renaming ren =
    adjustment (Adjustment.of_renaming ren)

  module Var = struct

    type 'ty t = 'ty var

    let fresh () =
      let constraints = [] in
      let state = Relaxed { constraints } in
      { state }

    module Constraints = struct

      type 'ty t = 'ty const =
        | Global of Location.t
        | Local of Location.t
        | Unused
        | Used_adjusted of ('ty Adjustment.t * Location.t) list

    end

    type use =
      | Use_global
      | Use_local
      | Use_unused
      | Use_adj of Adjustment.t list

    let use_of_ts ts =
      List.fold_left
        (fun use t ->
          match use, t with
          | _, Var v -> begin
              match use, pressue v with
              | Use_unused, _ -> Use_unused
              | _, Unused -> Use_unused
              | Use_global, _ -> Use_global
              | _, Global _ -> Use_global
              | use, Local _ -> use
              | Use_local, Used adjs ->
                  Use_adj (List.map (fun (adj, _) -> adj) adjs)
              | Uses adjs1, Uses adjs2 ->
                  let adjs =
                    List.concat_map
                      (fun adj1 ->
                        List.map
                          (fun (adj2, _) -> Adjustment.sequence adj1 adj2)
                          adjs2)
                      adjs1
                  in
                  if List.for_all Adjustment.is_identity adjs then Use_local
                  else Use_use adjs
            end
          | Use_unused, _ -> Use_unused
          | _, Md_unused -> Use_unused
          | Use_global, _ -> Use_global
          | _, Md_global -> Use_global
          | use, Md_local -> use
          | Use_local, Md_adjustment adj -> Use_adj [adj]
          | Use_adj adjs1, Md_adjustment adj2 ->
              let adjs =
                List.map (fun adj1 -> Adjustment.sequence adj1 adj2) adjs1
              in
              if List.for_all Adjustment.is_identity adjs then Use_local
              else Use_use adjs)
        Use_local ts
          
    and pressure var =
      match var.state with
      | Pressured consts -> consts
      | Escaped loc ->
          var.state <- Pressured (Global loc)
      | Relaxed { constraints } ->
          let consts =
            List.fold_left
             (fun constrs (ts, loc) ->
               match use_of_ts ts, constrs with
               | Use_unused, constrs -> constrs
               | Use_global, _ -> Global loc
               | _, Global _ as constrs -> constrs
               | Use_local, Unused -> Local loc
               | Use_local, Local _ as constrs -> constrs
               | Use_local, Used_adjusted adjs ->
                   let adjs = (Adjustment.identity, loc) :: adjs in
                   Used_adjusted adjs
               | Use_adj adj, Local loc' ->
                   let adjs = [adj, loc; Adjustment.identity, loc'] in
                   Used_adjusted adjs
               | Use_adj adj1, Used_adjusted adjs2 ->
                   let adjs = (adj1, loc) :: adjs2 in
                   Used_adjusted adjs)
             Unused constraints
          in
          var.state <- Pressured consts

    let rec is_obviously_global = function
      | [] -> false
      | Md_global :: _ -> true
      | _ :: rest -> is_obviously_global rest

    let is_identical ts1 ts2 =
      match ts1, ts2 with
      | [], [] -> true
      | t1 :: ts1, t2 :: ts2 ->
          t1 == t2 && is_identical ts1 ts2
      | [], _ :: _ | _ :: _, [] -> false    

    let add_constraint var ts loc =
      match var.state with
      | Pressured _ -> failwith "Adding constraint to pressured effect mode variable"
      | Escaped _ -> ()
      | Relaxed r ->
          if is_obviously_global ts then
            var.state <- Escaped loc
          else begin
            match r.constraints with
            | first :: _ when is_identical ts first -> ()
            | prev -> r.constraints <- (ts, loc) :: prev
          end
  end

end
