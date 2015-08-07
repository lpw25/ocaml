(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                 Mark Shinwell, Jane Street Europe                   *)
(*                                                                     *)
(*  Copyright 2013--2015, Jane Street Group, LLC                       *)
(*                                                                     *)
(*  Licensed under the Apache License, Version 2.0 (the "License");    *)
(*  you may not use this file except in compliance with the License.   *)
(*  You may obtain a copy of the License at                            *)
(*                                                                     *)
(*      http://www.apache.org/licenses/LICENSE-2.0                     *)
(*                                                                     *)
(*  Unless required by applicable law or agreed to in writing,         *)
(*  software distributed under the License is distributed on an        *)
(*  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,       *)
(*  either express or implied.  See the License for the specific       *)
(*  language governing permissions and limitations under the License.  *)
(*                                                                     *)
(***********************************************************************)

(* CR mshinwell: must ensure -allocation-profiling does not appear as a
   compiler option if WITH_ALLOCATION_PROFILING was not set *)

external annotation_of_value : 'a -> int
  = "caml_allocation_profiling_only_works_for_native_code"
    "caml_allocation_profiling_get_profinfo"

external dump_allocators_of_major_heap_blocks
   : filename:string
  -> dump_num_unaccounted_for:int
  -> unit
  = "caml_allocation_profiling_only_works_for_native_code"
    "caml_dump_allocators_of_major_heap_blocks_from_ocaml"

let dump_allocators_of_major_heap_blocks ?(dump_num_unaccounted_for = 0)
      ~filename () =
  dump_allocators_of_major_heap_blocks ~filename ~dump_num_unaccounted_for

external erase_profiling_annotations : unit -> unit
  = "caml_allocation_profiling_only_works_for_native_code"
    "caml_forget_where_values_were_allocated"

external dump_heapgraph : node_filename:string
  -> edge_filename:string
  -> unit
  = "caml_allocation_profiling_only_works_for_native_code"
    "caml_dump_heapgraph_from_ocaml"

external annotate_values_with_allocation_location : unit -> unit
  = "%identity" (*"caml_do_not_override_profinfo" "noalloc"*)

external annotate_values_with_given_integer : int -> bool
= "%identity"
(*
    = "caml_set_override_profinfo" "noalloc"
*)

let annotate_values_with_given_integer i =
  if i < 0 then `Out_of_range
  else if annotate_values_with_given_integer i then `Ok
  else `Out_of_range

external max_annotation_value : unit -> int
= "%identity"
(*
  = "caml_allocation_profiling_max_override_profinfo" "noalloc"
*)

(*
external dump_backtraces_to_file : filename:string -> unit
  = "caml_allocation_profiling_dump_backtraces_to_file"
*)

type profinfo = int

external min_override_profinfo : unit -> profinfo
  = "caml_allocation_profiling_only_works_for_native_code"
    "caml_allocation_profiling_min_override_profinfo" "noalloc"

external max_override_profinfo : unit -> profinfo
  = "caml_allocation_profiling_only_works_for_native_code"
    "caml_allocation_profiling_max_override_profinfo" "noalloc"

external profinfo_none : unit -> profinfo
  = "caml_allocation_profiling_only_works_for_native_code"
    "caml_allocation_profiling_profinfo_none" "noalloc"

external profinfo_overflow : unit -> profinfo
  = "caml_allocation_profiling_only_works_for_native_code"
    "caml_allocation_profiling_profinfo_none" "noalloc"

external get_profinfo : 'a -> profinfo
  = "caml_allocation_profiling_only_works_for_native_code"
    "caml_allocation_profiling_get_profinfo" "noalloc"

external trie_is_initialized : unit -> bool
  = "caml_allocation_profiling_only_works_for_native_code"
    "caml_allocation_profiling_trie_is_initialized" "noalloc"

external get_trie_root : unit -> Obj.t
  = "caml_allocation_profiling_only_works_for_native_code"
    "caml_allocation_profiling_get_trie_root" "noalloc"

(*
let debug () =
  Printf.printf "trie initialized? %b\n%!" (trie_is_initialized ())
*)

external debug : unit -> unit
  = "caml_allocation_profiling_only_works_for_native_code"
    "caml_allocation_profiling_debug" "noalloc"

external marshal_trie : out_channel -> unit
  = "caml_allocation_profiling_only_works_for_native_code"
    "caml_allocation_profiling_marshal_trie"

type trie_node = Obj.t array

type trie =
  | Empty
  | Non_empty of trie_node

let unmarshal_trie in_channel =
  let trie = Marshal.from_channel in_channel in
  if trie = () then
    Empty
  else
    Non_empty ((Obj.magic trie) : trie_node)
