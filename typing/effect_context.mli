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

type 'a t =
  { effects : (string * 'a option) list }

val hash_name : string -> int

type join_error =
  | Different_effect_names of string * string

val join :
  (string -> 'a -> 'a -> 'a)
  -> 'a t
  -> 'a t
  -> ('a, join_error) Result.t

type position =
  | First
  | Second

type equal_error =
  | Different_effect_names of string * string
  | Missing_effect of position * string

val equal :
  ('b -> string -> 'a -> 'a -> 'b)
  -> 'b
  -> 'a t
  -> 'a t
  -> ('b, equal_error) Result.t

type subeffect_error =
  | Different_effect_names of string * string
  | Missing_effect of string

val subeffect :
  (string -> 'a -> 'a -> unit)
  -> 'a t
  -> 'a t
  -> (unit, subeffect_error) Result.t

val filter : string -> 'a t -> ('a * 'a t, join_error) Result.t

module Renaming : sig

  type 'a ctx := 'a t

  type t

  val apply : (string -> 'a -> 'a -> 'a) -> 'a ctx -> t -> 'a ctx

  val compose : t -> t -> t

  val normalize : t -> t

  val expected_context : (unit -> 'a) -> t -> 'a ctx

  type parse_error =
    | Multiply_bound_variable of string loc
    | Unmatched_effect_renaming_binding of string loc
    | Unused_effect_renaming_binding of string loc

  val parse :
    outer:string loc option list
    -> inner:string loc list
    -> (t, parse_error) Result.t

  module Desc : sig

    type t =
      { outer : string option list;
        inner : string list; }

  end

  val desc : t -> Desc.t

end

module Adjustment : sig

  type 'a ctx := 'a t

  type 'a t =
    | Bottom
    | Update of
        { adjustment : Adjustment.t;
          addition : (string * 'a) list; }

  type equal_error =
    | Different_effect_names of string * string
    | Missing_effect of position * string
    | Missing_input_effect 

  val equal :
    ('b -> string -> 'a -> 'a -> 'b)
    -> 'b
    -> 'a t
    -> 'a t
    -> ('b, equal_error) Result.t

  val is_identity : 'a t -> bool

  val add_extension : 'a t -> (string * 'a) list -> 'a t

  val add_renaming : 'a t -> Renaming.t -> 'a t

end
