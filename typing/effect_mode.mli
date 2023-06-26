
type position = First | Second

module Renaming : sig

  type t

  type parse_error =
    | Multiply_bound_variable of string Location.loc
    | Unbound_variable of string Location.loc
    | Multiply_used_variable of string Location.loc
    | Unused_variable of string Location.loc

  type outer = string Location.loc option

  type inner = string Location.loc

  val parse :
    outer: (string * outer) list
    -> inner: (string * inner) list
    -> (t, parse_error) Result.t

  val apply :
    (string -> 'ty -> 'ty -> unit) -> 'ty Effect_context.t -> t -> 'ty Effect_context.t

  val compose : t -> t -> t

end

module Adjustment : sig

  type 'ty t

  type parse_error =
    | Multiply_bound_variable of string Location.loc
    | Unbound_variable of string Location.loc
    | Multiply_used_variable of string Location.loc
    | Unused_variable of string Location.loc

  type outer = string Location.loc option

  type 'ty inner =
    | Rename of string Location.loc
    | Bind of 'ty

  val parse :
    outer:(string * outer) list
    -> inner:(string * 'ty inner) list
    -> ('ty t, parse_error) Result.t

  module Desc : sig

    type outer = string option

    type 'ty inner =
      | Rename of string
      | Bind of 'ty option

    type 'ty t =
      { outer : (string * outer) list;
        inner : (string * 'ty inner) list; }

  end

  val desc : 'ty t -> 'ty Desc.t

  type apply_error =
    | Different_effect_names of string * string

  val apply :
    ('ty -> 'ty -> unit)
    -> 'ty t
    -> 'ty Effect_context.t
    -> ('ty Effect_context.t, apply_error) Result.t

  type equal_error =
    | Different_input_names of string * string * int
    | Different_output_names of
        { origin : string;
          index : int;
          first_destination : string;
          second_destination : string;
          destination_index : int; }
    | Missing_bind of string * int * position
    | Missing_rename of string * int * position
    | Different_renames of
        { origin : string;
          origin_index : int;
          first_destination : string;
          first_destination_index : int;
          second_destination : string;
          second_destination_index : int; }

    val equal :
      ('a -> string -> 'ty -> 'ty -> 'a)
      -> 'a
      -> 'ty t
      -> 'ty t
      -> ('a, equal_error) Result.t

  val iter : ('ty -> unit) -> 'ty t -> unit

  val fold : ('a -> 'ty -> 'a) -> 'a -> 'ty t -> 'a

  val copy : ('ty -> 'ty) -> 'ty t -> 'ty t

  val copy_fold :
    ('a -> 'ty -> 'a * 'ty) -> 'a -> 'ty t -> 'a * 'ty t

  module Simplified : sig

    type 'ty t
    (** Simplified adjustments ignore names and whether a transparent
        renaming is explicit or not. *)

    type equal_error =
      | Missing_bind of string * int * position
      | Different_renames of
          { origin : string;
            origin_index : int;
            first_destination : string;
            first_destination_index : int;
            second_destination : string;
            second_destination_index : int; }

    val equal :
      ('a -> string -> 'ty -> 'ty -> 'a)
      -> 'a
      -> 'ty t
      -> 'ty t
      -> ('a, equal_error) Result.t

  end

  val simplify : 'ty t -> 'ty Simplified.t

end

module Mode : sig

  type 'ty t

  module Var : sig

    type 'ty mode := 'ty t

    type 'ty t

    val fresh : unit -> 'ty t

    module Constraints : sig

      type 'ty t =
        | Global of Location.t
        | Local of Location.t
        | Unused
        | Used_adjusted of ('ty Adjustment.Simplified.t * Location.t) list

    end

    val pressure : 'ty t -> 'ty Constraints.t
    (** [pressure t] returns the constraints on [t]. Attempts to add
        constraints to a variable after it has been pressured will fail
        with an exception. *)

    val add_constraint : 'ty t -> 'ty mode list -> Location.t -> unit
    (** [add_constraint t [a; b; c]] adds a constraint that [t = a . b . c].
        Note that pressuring [t] will now pressure [a], [b] and [c] as
        well. *)

  end

  val var : 'ty Var.t -> 'ty t

  val global : 'ty t

  val unused : 'ty t

  val adjustment : 'ty Adjustment.t -> 'ty t

  val extension : 'ty Effect_context.t -> 'ty t

  val renaming : Renaming.t -> 'ty t

end
