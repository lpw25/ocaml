
module Adjustment : sig

  type 'ty t

  module Parse_error : sig

    type t =
      | Multiple_outer_uses of string
      | No_outer_uses of string
      | Multiple_inner_uses of string
      | No_inner_uses of string

    val print : Format.formatter -> t -> unit

  end

  type outer = string option

  type 'ty inner =
    | Rename of string
    | Bind of 'ty option

  val parse :
    outer:(string * outer) list
    -> inner:(string * 'ty inner) list
    -> ('ty t, Parse_error.t) Result.t

  val handler : 'ty Effect_context.t -> 'ty t

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

    val swap_position : t -> t

  end

  val apply :
    equal:(string -> int -> 'ty -> 'ty -> unit)
    -> 'ty t
    -> 'ty Effect_context.t
    -> ('ty Effect_context.t, Apply_error.t) Result.t

  val expected_context : (unit -> 'ty) -> 'ty t -> 'ty Effect_context.t

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
    equal:('a -> string -> int -> 'ty -> 'ty -> 'a)
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

    type 'ty adjustment := 'ty t

    type 'ty t
    (** Simplified adjustments ignore names and whether a transparent
        renaming is explicit or not. *)

    val equal :
      equal:('a -> string -> int -> 'ty -> 'ty -> 'a)
      -> 'a
      -> 'ty t
      -> 'ty t
      -> ('a, Equal_error.t) Result.t

    val join :
      join:(string -> int -> 'ty -> 'ty -> 'ty)
      -> 'ty t
      -> 'ty t
      -> 'ty t

    module Action : sig

      type rename =
        { tag : int;
          depth : int; }

      type t =
        { renames : rename option list;
          shift : int; }

    end

    val fold_actions : ('a -> Action.t -> 'a) -> 'a -> 'ty t -> 'a

    val reify : 'ty t -> 'ty adjustment

  end

  val simplify : 'ty t -> 'ty Simplified.t

end

module Mode : sig

  type ('ty, 'lcl) t

  module Usage : sig

    type ('ty, 'lcl) t

    module Adjusted_match_error : sig

      type ('ty, 'lcl) t =
        | Escaping
        | Adjusted of 'ty Adjustment.Simplified.t * Adjustment.Equal_error.t

    end

    val matches_adjusted :
      equal:(string -> int -> 'ty -> 'ty -> unit)
      -> is_local:('lcl -> unit)
      -> 'ty Adjustment.Simplified.t
      -> ('ty, 'lcl) t
       -> (unit, ('ty, 'lcl) Adjusted_match_error.t) Result.t

    module Unadjusted_match_error : sig

      type ('ty, 'lcl) t =
        | Adjusted of 'ty Adjustment.Simplified.t

    end

    val matches_unadjusted :
      submode:('lcl -> 'lcl -> unit)
      -> is_global:('lcl -> unit)
      -> ('ty, 'lcl) t
      -> 'lcl
      -> (unit, ('ty, 'lcl) Unadjusted_match_error.t) Result.t

  end

  module Var : sig

    type ('ty, 'lcl) mode := ('ty, 'lcl) t

    type ('ty, 'lcl) t

    val fresh : unit -> ('ty, 'lcl) t

    val pressure : ('ty, 'lcl) t -> (('ty, 'lcl) Usage.t * Location.t) list
    (** [pressure t] returns the uses on [t]. Attempts to add
        uses to a variable after it has been pressured will fail
        with an exception. *)

    val use : ('ty, 'lcl) t -> ('ty, 'lcl) mode -> Location.t -> unit
    (** [add_use t m] adds a uses of [t] at mode [m].  Note that
        pressuring [t] will now pressure [m] as well. *)

  end

  val var : ('ty, 'lcl) Var.t -> ('ty, 'lcl) t

  val escaping : ('ty, 'lcl) t

  val unused : ('ty, 'lcl) t

  val local : ('ty, 'lcl) t

  val unadjusted : 'lcl -> ('ty, 'lcl) t

  val adjusted : 'ty Adjustment.t -> ('ty, 'lcl) t

  val times : ('ty, 'lcl) t -> ('ty, 'lcl) t -> ('ty, 'lcl) t

end
