(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Auxiliaries for type-based optimizations, e.g. array kinds *)

val has_base_type : Typedtree.expression -> Path.t -> bool

val maybe_addr_type : Env.t -> Types.type_expr -> Lambda.maybe_addr
val maybe_addr : Typedtree.expression -> Lambda.maybe_addr

val maybe_addr_array_type : Env.t -> Types.type_expr -> Lambda.maybe_addr
val maybe_addr_array : Typedtree.expression -> Lambda.maybe_addr

val bigarray_kind_and_layout :
      Typedtree.expression -> Lambda.bigarray_kind * Lambda.bigarray_layout
