type empty = |

type position = First | Second

module Adjustment = struct

  type 'ty adjust =
    | Rename of
        { to_hash : int;
          to_index : int; }
    | Bind of 'ty option

  type 'ty gen =
    { image : string array Int.Map.t;
      adjustments : (string * 'ty adjust) array Int.Map.t }

  type 'ty t = 'ty option gen

  let identity =
    let image = Int.Map.empty in
    let adjustments = Int.Map.empty in
    { image; adjustments }

  module Parse_error = struct

    type t =
      | Multiple_outer_uses of string
      | No_outer_uses of string
      | Multiple_inner_uses of string
      | No_inner_uses of string

    let print ppf = function
      | Multiple_outer_uses name ->
          Format.fprintf ppf
            "%s appears multiple times in the outer context"
            name
      | No_outer_uses name ->
          Format.fprintf ppf
            "%s does not appear in the outer context"
            name
      | Multiple_inner_uses name ->
          Format.fprintf ppf
            "%s appears multiple times in the inner context"
            name
      | No_inner_uses name ->
          Format.fprintf ppf
            "%s does not appear in the inner context"
            name
  end

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
    in
    let adjustments =
      Int.Map.map
        (fun rev_adjusts -> Array.of_list (List.rev rev_adjusts))
        rev_adjustments
    in
    { image; adjustments }

  let expected_context newvar t =
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
              | Bind tyo ->
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

    type 'ty gen = 'ty gen

    type 'ty t = 'ty option gen

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

  module Usage = struct

    type 'ty bind =
      | Different_renames of
          { first_destination : string;
            first_destination_index : int;
            second_destination : string;
            second_destination_index : int; }
      | Missing_bind
      | Bind of 'ty option list

    type ('ty, 'lcl) t =
      | Escaping
      | Used of
          { adjustment : 'ty bind Adjustment.Simplified.gen;
            localities : 'lcl list; }
      | Used_unequal of
          { first_adjustment : 'ty bind Adjustment.Simplified.gen;
            second_adjustment : 'ty bind Adjustment.Simplified.gen;
            localities : 'lcl list; }

    let escaping = Escaping

    let unadujusted lcl =
      let adjustment = Adjustment.Simplified.identity in
      let localities = [lcl] in
      Used { adjustment; localities }

    let adjusted adjustment =
      let localities = [] in
      Used { adjustment; localities }

    let zero = Unused

    let one =
      let adjustment = Adjustment.Simplified.identity in
      let localities = [] in
      Used { adjustment; localities }

    let times t1 t2 =
      match t1, t2 with
      | Unused, _ | _, Unused -> Unused
      | Escaping, _ | _, Escaping -> Escaping
      | 

    let meet to1 t2 =
      match to1 with
      | None -> t2
      | Some t1 ->
          match t1, t2 with
          | ....

  end

  type ('ty, 'lcl) t =
    | Var of ('ty, 'lcl) var
    | Escaping
    | Unadjusted of 'lcl
    | Adjusted of 'ty Adjustment.Simplified.t
    | Zero
    | One
    | Times of ('ty, 'lcl) t * ('ty, 'lcl) t

  and ('ty, 'lcl) var =
    { mutable state : ('ty, 'lcl) state }

  and ('ty, 'lcl) state =
    | Forced of
        { uses : (('ty, 'lcl) Usage.t * Location.t) list;
          usage : ('ty, 'lcl) Usage.t; }
    | Pressured of (('ty, 'lcl) Usage.t * Location.t) list
    | Escaped of Location.t
    | Relaxed of
        { mutable uses : (('ty, 'lcl) t * Location.t) list }

  let var v = Var v

  let escaping = Escaping

  let unadjusted lcl = Unadjusted lcl

  let adjusted adj = Adjusted adj

  let local = One

  let unused = Zero

  let times t1 t2 =
    match t1, t2 with
    | Zero, _ | _, Zero -> Zero
    | One, t -> t
    | t, One -> t
    | _, _ -> Times (t1, t2)

  module Var = struct

    type ('ty, 'lcl) t = ('ty, 'lcl) var

    let fresh () =
      let constraints = [] in
      let state = Relaxed { constraints } in
      { state }

    let usage_of_mode = function
      | Var var -> force var
      | Escaped -> Usage.escaped
      | Unadjusted lcl -> Usage.unadjusted lcl
      | Adjusted adj -> Usage.adjusted adj
      | Zero -> Usage.zero
      | One -> Usage.one
      | Times(t1, t2) -> Usage.times t1 t2

    and pressure var =
      match var.state with
      | Forced { uses; _ } -> uses
      | Pressured uses -> uses
      | Escaped loc ->
          let uses = [Escaped, loc] in
          var.state <- Pressured uses;
          uses
      | Relaxed { uses } ->
          let uses =
            List.map
              (fun (t, loc) ->
                let usage = usage_of_mode loc t in
                usage, loc)
              uses
          in
          var.state <- Pressured uses;
          uses

    and force var =
      match var.state with
      | Forced { usage; _ } -> usage
      | _ ->
          let uses = pressure var in
          let usage =
            List.fold_left
              (fun acc (usage, _) -> Usage.meet acc usage)
              None uses
          in
          var.state <- Forced { uses; usage };
          usage

    let is_unused = function
      | Unused -> true
      | _ -> false

    let is_escaping = function
      | Escaping -> true
      | _ -> false

    let is_identical t1 t2 =
      match ts1, ts2 with
      | [], [] -> true
      | t1 :: ts1, t2 :: ts2 ->
          t1 == t2 && is_identical ts1 ts2
      | [], _ :: _ | _ :: _, [] -> false    

    let use var t loc =
      if is_unused t then ()
      else begin
        match var.state with
        | Forced _ | Pressured _ ->
            failwith "Adding use to pressured effect mode variable"
        | Escaped _ -> ()
        | Relaxed r ->
            if is_escaping t then begin
              var.state <- Escaped loc
            end else begin
              match r.uses with
              | first :: _ when is_identical ts first -> ()
              | prev -> r.uses <- (ts, loc) :: prev
            end
      end

  end

end
