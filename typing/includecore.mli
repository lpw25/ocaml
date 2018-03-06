(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Inclusion checks for the core language *)

(* open Asttypes *)
open Typedtree
open Types

exception Dont_match

type type_mismatch =
    Arity
  | Privacy
  | Kind
  | Constraint
  | Manifest
  | Variance
  | Sort of bool * bool
  | Param_sort of int * bool * bool
  | Field_type of Ident.t
  | Field_mutable of Ident.t
  | Field_arity of Ident.t
  | Field_names of int * Ident.t * Ident.t
  | Field_missing of bool * Ident.t
  | Record_representation of bool

(* type effect_mismatch =
 *   | Effect_kind
 *   | Effect_manifest
 *   | Effect_constructor_arg_type of label
 *   | Effect_constructor_ret_type of label
 *   | Effect_constructor_arity of label
 *   | Effect_constructor_names of int * label * label
 *   | Effect_constructor_missing of bool * label *)

val value_descriptions:
    Env.t -> value_description -> value_description -> module_coercion
val type_declarations:
    ?equality:bool ->
      Env.t -> string ->
        type_declaration -> Ident.t -> type_declaration -> type_mismatch list
val extension_constructors:
    Env.t -> Ident.t -> extension_constructor -> extension_constructor -> bool
(* val effect_declarations:
 *       Env.t -> string ->
 *         effect_declaration -> Ident.t -> effect_declaration -> effect_mismatch list *)

(*
val class_types:
        Env.t -> class_type -> class_type -> bool
*)

val report_type_mismatch:
    string -> string -> string -> Format.formatter -> type_mismatch list -> unit
(* val report_effect_mismatch:
 *     string -> string -> string -> Format.formatter -> effect_mismatch list -> unit *)
