(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Format

type t =
  | Unit of Unit_name.t
  | Stamped of { stamp: int; name: string; global: bool }

(* A stamp of 0 denotes a persistent identifier *)

let currentstamp = ref 0

let create s =
  incr currentstamp;
  Stamped { name = s; stamp = !currentstamp; global = false }

let create_global s =
  incr currentstamp;
  Stamped { name = s; stamp = !currentstamp; global = true }

let create_unit uname =
  Unit uname

let rename i =
  match i with
  | Stamped r ->
      incr currentstamp;
      Stamped { r with stamp = !currentstamp }
  | Unit _ ->
      Misc.fatal_error
        "Ident.rename: unit identifier"

let name = function
  | Unit uname -> Unit_name.name uname
  | Stamped { name } -> name

let stamp = function
  | Unit _ -> 0
  | Stamped { stamp } -> stamp

let unique_name = function
  | Unit uname -> Unit_name.name uname
  | Stamped { name; stamp } ->
      name ^ "_" ^ string_of_int stamp

let unique_toplevel_name = function
  | Unit uname -> Unit_name.name uname
  | Stamped { name; stamp } ->
      name ^ "/" ^ string_of_int stamp

let unit = function
  | Unit _ -> true
  | Stamped _ -> false

let unit_name = function
  | Unit uname -> uname
  | Stamped _ ->
      Misc.fatal_error "Ident.unit_name: non-unit identifier"

let equal i1 i2 = (name i1) = (name i2)

let same i1 i2 =
  match i1, i2 with
  | Unit un1, Unit un2 -> Unit_name.equal un1 un2
  | Stamped { stamp = stamp1 }, Stamped { stamp = stamp2 } -> stamp1 = stamp2
  | _, _ -> false

let compare i1 i2 =
  match i1, i2 with
  | Unit un1, Unit un2 ->
      Unit_name.compare un1 un2
  | Unit _, Stamped _ -> 1
  | Stamped s1, Stamped s2 ->
      let c = Pervasives.compare s1.stamp s2.stamp in
        if c <> 0 then c
        else Pervasives.compare s1.name s2.name
  | Stamped _, Unit _ -> -1

let binding_time i = stamp i

let current_time() = !currentstamp
let set_current_time t = currentstamp := max !currentstamp t

let reinit_level = ref (-1)

let reinit () =
  if !reinit_level < 0
  then reinit_level := !currentstamp
  else currentstamp := !reinit_level

let hide i =
  match i with
  | Stamped r -> Stamped { r with stamp = -1 }
  | Unit _ ->
      Misc.fatal_error
        "Ident.hide: unit identifier"

let global = function
  | Unit _ -> true
  | Stamped { global } -> global

let print ppf = function
  | Unit uname -> fprintf ppf "%s!" (Unit_name.name uname)
  | Stamped { name; stamp; global } ->
      if stamp = -1 then fprintf ppf "%s#" name
      else fprintf ppf "%s/%i%s" name stamp
                   (if global then "g" else "")

type 'a tbl =
    Empty
  | Node of 'a tbl * 'a data * 'a tbl * int

and 'a data =
  { ident: t;
    data: 'a;
    previous: 'a data option }

let empty = Empty

(* Inline expansion of height for better speed
 * let height = function
 *     Empty -> 0
 *   | Node(_,_,_,h) -> h
 *)

let mknode l d r =
  let hl = match l with Empty -> 0 | Node(_,_,_,h) -> h
  and hr = match r with Empty -> 0 | Node(_,_,_,h) -> h in
  Node(l, d, r, (if hl >= hr then hl + 1 else hr + 1))

let balance l d r =
  let hl = match l with Empty -> 0 | Node(_,_,_,h) -> h
  and hr = match r with Empty -> 0 | Node(_,_,_,h) -> h in
  if hl > hr + 1 then
    match l with
    | Node (ll, ld, lr, _)
      when (match ll with Empty -> 0 | Node(_,_,_,h) -> h) >=
           (match lr with Empty -> 0 | Node(_,_,_,h) -> h) ->
        mknode ll ld (mknode lr d r)
    | Node (ll, ld, Node(lrl, lrd, lrr, _), _) ->
        mknode (mknode ll ld lrl) lrd (mknode lrr d r)
    | _ -> assert false
  else if hr > hl + 1 then
    match r with
    | Node (rl, rd, rr, _)
      when (match rr with Empty -> 0 | Node(_,_,_,h) -> h) >=
           (match rl with Empty -> 0 | Node(_,_,_,h) -> h) ->
        mknode (mknode l d rl) rd rr
    | Node (Node (rll, rld, rlr, _), rd, rr, _) ->
        mknode (mknode l d rll) rld (mknode rlr rd rr)
    | _ -> assert false
  else
    mknode l d r

let rec add id data = function
    Empty ->
      Node(Empty, {ident = id; data = data; previous = None}, Empty, 1)
  | Node(l, k, r, h) ->
      let c = String.compare (name id) (name k.ident) in
      if c = 0 then
        Node(l, {ident = id; data = data; previous = Some k}, r, h)
      else if c < 0 then
        balance (add id data l) k r
      else
        balance l k (add id data r)

let rec find_stamp s = function
    None ->
      raise Not_found
  | Some k ->
      if stamp k.ident = s then k.data else find_stamp s k.previous

let rec find_same id = function
    Empty ->
      raise Not_found
  | Node(l, k, r, _) ->
      let c = String.compare (name id) (name k.ident) in
      if c = 0 then
        if stamp id = stamp k.ident
        then k.data
        else find_stamp (stamp id) k.previous
      else
        find_same id (if c < 0 then l else r)

let rec find_name n = function
    Empty ->
      raise Not_found
  | Node(l, k, r, _) ->
      let c = String.compare n (name k.ident) in
      if c = 0 then
        k.data
      else
        find_name n (if c < 0 then l else r)

let rec get_all = function
  | None -> []
  | Some k -> k.data :: get_all k.previous

let rec find_all n = function
    Empty ->
      []
  | Node(l, k, r, _) ->
      let c = String.compare n (name k.ident) in
      if c = 0 then
        k.data :: get_all k.previous
      else
        find_all n (if c < 0 then l else r)

let rec fold_aux f stack accu = function
    Empty ->
      begin match stack with
        [] -> accu
      | a :: l -> fold_aux f l accu a
      end
  | Node(l, k, r, _) ->
      fold_aux f (l :: stack) (f k accu) r

let fold_name f tbl accu = fold_aux (fun k -> f k.ident k.data) [] accu tbl

let rec fold_data f d accu =
  match d with
    None -> accu
  | Some k -> f k.ident k.data (fold_data f k.previous accu)

let fold_all f tbl accu =
  fold_aux (fun k -> fold_data f (Some k)) [] accu tbl

(* let keys tbl = fold_name (fun k _ accu -> k::accu) tbl [] *)

let rec iter f = function
    Empty -> ()
  | Node(l, k, r, _) ->
      iter f l; f k.ident k.data; iter f r

(* Idents for sharing keys *)

(* They should be 'totally fresh' -> neg numbers *)
let key_name = ""

let make_key_generator () =
  let c = ref 1 in
  function
  | Stamped r ->
      let stamp = !c in
      decr c ;
      Stamped { r with name = key_name; stamp = stamp; }
  | Unit _ ->
      Misc.fatal_error "Ident.make_key_generator: unit identifier"

let output oc id = output_string oc (unique_name id)
let hash i = (Char.code (name i).[0]) lxor (stamp i)

let original_equal = equal
include Identifiable.Make (struct
  type nonrec t = t
  let compare = compare
  let output = output
  let print = print
  let hash = hash
  let equal = same
end)
let equal = original_equal
