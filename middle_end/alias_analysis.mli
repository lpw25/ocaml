(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

type allocation_point =
  | Symbol of Symbol.t
  | Variable of Variable.t

type allocated_const =
  | Normal of Allocated_const.t
  | Array of Lambda.array_kind * Asttypes.mutable_flag * Variable.t list
  | Duplicate_array of Lambda.array_kind * Asttypes.mutable_flag * Variable.t

type constant_defining_value =
  | Allocated_const of allocated_const
  | Block of Tag.t * Variable.t list
  | Set_of_closures of Flambda.set_of_closures
  | Project_closure of Flambda.project_closure
  | Move_within_set_of_closures of Flambda.move_within_set_of_closures
  | Project_var of Flambda.project_var
  | Field of Variable.t * int
  | Symbol_field of Symbol.t * int
  | Const of Flambda.const
  | Symbol of Symbol.t
  | Variable of Variable.t

type initialize_symbol_field = Variable.t option

(** Simple alias analysis working over information about which
    symbols have been assigned to variables; and which constants have
    been assigned to symbols.  The return value gives the assignment
    of the defining values of constants to variables.
    Also see comments for [Lift_constants], whose input feeds this
    pass. *)
val run
   : constant_defining_value Variable.Tbl.t
  -> initialize_symbol_field list Symbol.Tbl.t
  -> Flambda.constant_defining_value Symbol.Tbl.t
  -> allocation_point Variable.Map.t

val print_constant_defining_value
   : Format.formatter
  -> constant_defining_value
  -> unit
