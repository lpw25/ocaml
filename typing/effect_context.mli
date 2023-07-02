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

val hash_name : string -> int

type 'ty t

val create : (string * 'ty) list -> 'ty t

val append : (string * 'ty) list -> 'ty t -> 'ty t

val empty : 'ty t

val is_empty : 'ty t -> bool

module Join_error : sig

  type t =
    | Different_effect_names of string * string

  val swap_position : t -> t

end

val join :
  (string -> 'ty -> 'ty -> 'ty)
  -> 'ty t
  -> 'ty t
  -> ('ty t, Join_error.t) Result.t

module Equal_error : sig

  type t =
    | Different_effect_names of string * string
    | Missing_effect of Misc.position * string

  val swap_position : t -> t

end

val equal :
  ('a -> string -> 'ty -> 'ty -> 'a)
  -> 'a
  -> 'ty t
  -> 'ty t
  -> ('a, Equal_error.t) Result.t

module Subeffect_error : sig

  type t =
    | Different_effect_names of string * string
    | Missing_effect of string

  val swap_position : t -> t

end

val subeffect :
  (string -> 'ty -> 'ty -> unit)
  -> 'ty t
  -> 'ty t
  -> (unit, Subeffect_error.t) Result.t

type handle_error =
  | Different_effect_names of string * string

val handle : 
  string
  -> 'ty t
  -> ('ty * 'ty t, handle_error) Result.t

val iter : ('ty -> unit) -> 'ty t -> unit

val fold : ('a -> 'ty -> 'a) -> 'a -> 'ty t -> 'a

val copy : ('ty -> 'ty) -> 'ty t -> 'ty t

val copy_fold :
  ('a -> 'ty -> 'a * 'ty) -> 'a -> 'ty t -> 'a * 'ty t

module Desc : sig

  type 'ty t = (string * 'ty option) list

  val map : ('a -> 'b) -> 'a t -> 'b t

end

val desc : 'ty t -> 'ty Desc.t
