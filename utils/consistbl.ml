(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Consistency tables: for checking consistency of module CRCs *)

type t = (Digest.t * string) Unit_name.Tbl.t

let create () = Unit_name.Tbl.create 13

let clear = Unit_name.Tbl.clear

exception Inconsistency of Unit_name.t * string * string

exception Not_available of Unit_name.t

let check tbl name crc source =
  try
    let (old_crc, old_source) = Unit_name.Tbl.find tbl name in
    if crc <> old_crc then raise(Inconsistency(name, source, old_source))
  with Not_found ->
    Unit_name.Tbl.add tbl name (crc, source)

let check_noadd tbl name crc source =
  try
    let (old_crc, old_source) = Unit_name.Tbl.find tbl name in
    if crc <> old_crc then raise(Inconsistency(name, source, old_source))
  with Not_found ->
    raise (Not_available name)

let set tbl name crc source = Unit_name.Tbl.add tbl name (crc, source)

let source tbl name = snd (Unit_name.Tbl.find tbl name)

let extract (l : Unit_name.t list) (tbl : t) =
  let l = List.sort_uniq Unit_name.compare l in
  List.fold_left
    (fun assc name ->
       try
         let (crc, _) = Unit_name.Tbl.find tbl name in
           (name, Some crc) :: assc
       with Not_found ->
         (name, None) :: assc)
    [] l

let filter p tbl =
  let to_remove = ref [] in
  Unit_name.Tbl.iter
    (fun name _ ->
      if not (p name) then to_remove := name :: !to_remove)
    tbl;
  List.iter
    (fun name ->
       while Unit_name.Tbl.mem tbl name do
         Unit_name.Tbl.remove tbl name
       done)
    !to_remove
