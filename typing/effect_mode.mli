
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

  module Desc : sig

    type outer = string option

    type inner = string

    type t =
      { outer : (string * outer) list;
        inner : (string * inner) list; }

  end

  val desc : t -> Desc.t

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
    | Bind of 'ty option

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

    val map : ('a -> 'b) -> 'a t -> 'b t

  end

  val desc : 'ty t -> 'ty Desc.t

  module Apply_error : sig

    type t =
      | Different_effect_names of string * string

  end

  val apply :
    (string -> int -> 'ty -> 'ty -> unit)
    -> 'ty t
    -> 'ty Effect_context.t
    -> ('ty Effect_context.t, Apply_error.t) Result.t


  module Equal_error : sig

    type t =
      | Different_input_names of string * string * int
      | Missing_bind of string * int * Misc.position
      | Missing_rename of string * int * Misc.position
      | Different_renames of
          { origin : string;
            origin_index : int;
            first_destination : string;
            first_destination_index : int;
            second_destination : string;
            second_destination_index : int; }

    val swap_position : t -> t

  end

  val equal :
    ('a -> string -> int -> 'ty -> 'ty -> 'a)
    -> 'a
    -> 'ty t
    -> 'ty t
    -> ('a, Equal_error.t) Result.t

  val iter : ('ty -> unit) -> 'ty t -> unit

  val fold : ('a -> 'ty -> 'a) -> 'a -> 'ty t -> 'a

  val copy : ('ty -> 'ty) -> 'ty t -> 'ty t

  val copy_fold :
    ('a -> 'ty -> 'a * 'ty) -> 'a -> 'ty t -> 'a * 'ty t

  module Simplified : sig

    type 'ty t
    (** Simplified adjustments ignore names and whether a transparent
        renaming is explicit or not. *)

    module Equal_error : sig

      type t =
        | Missing_bind of string * int * Misc.position
        | Different_renames of
            { origin : string;
              origin_index : int;
              first_destination : string;
              first_destination_index : int;
              second_destination : string;
              second_destination_index : int; }

    end

    val equal :
      ('a -> string -> int -> 'ty -> 'ty -> 'a)
      -> 'a
      -> 'ty t
      -> 'ty t
      -> ('a, Equal_error.t) Result.t

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
