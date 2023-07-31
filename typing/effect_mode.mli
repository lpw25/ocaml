
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

  val handler : (string * 'ty) list -> 'ty t

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
    equal:(string -> int -> 'ty1 -> 'ty2 -> unit)
    -> 'ty1 t
    -> 'ty2 Effect_context.t
    -> ('ty2 Effect_context.t, Apply_error.t) Result.t

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
    equal:('a -> string -> int -> 'ty1 -> 'ty2 -> 'a)
    -> 'a
    -> 'ty1 t
    -> 'ty2 t
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
      equal:('a -> string -> int -> 'ty1 -> 'ty2 -> 'a)
      -> 'a
      -> 'ty1 t
      -> 'ty2 t
      -> ('a, Equal_error.t) Result.t

    val compose : 'ty t -> 'ty t -> 'ty t

    val right_residual : 'ty1 t -> 'ty2 t -> 'ty1 t

    val reify : 'ty t -> 'ty adjustment

  end

  val simplify : 'ty t -> 'ty Simplified.t

end

module Mode : sig

  type ('ty, 'lcl) t

  val escaping : ('ty, 'lcl) t

  val unused : ('ty, 'lcl) t

  val local : ('ty, 'lcl) t

  val unadjusted : 'lcl -> ('ty, 'lcl) t

  val adjusted : 'ty Adjustment.t -> ('ty, 'lcl) t

  val join : ('ty, 'lcl) t -> ('ty, 'lcl) t -> ('ty, 'lcl) t

  val right_residual : ('ty1, 'lcl) t -> ('ty2, 'lcl) t -> ('ty1, 'lcl) t

  module Factor : sig
    (** Effect modes do not in general have a left residual. Instead we
        provide a restricted form: we restrict the compose and left
        residual operations to work with a [Factor.t] type that behaves
        as a resource (i.e. each call to the [create] functions creates
        a fresh factor not equal to any other). Composition by a factor
        is not defined on the left residuals of other factors. Since we
        only need to use left residuals and composition according to a
        stack discipline, this is sufficient for our needs. *)

    type ('ty, 'lcl) mode := ('ty, 'lcl) t

    type ('ty, 'lcl) t

    module Var : sig

      type ('ty, 'lcl) t

      val create : unit -> ('ty, 'lcl) t

      val consume : ('ty, 'lcl) t -> ('ty, 'lcl) mode
      (** [consumes t] returns the value of [t] and marks it as
          consumed. Any further attempts to constraint it with
          [submode] will fail with an exception *)

    end

    module Adjustment : sig

      type ('ty, 'lcl) t

      val create : 'ty Adjustment.Simplified.t -> ('ty, 'lcl) t

    end

    val var : ('ty, 'lcl) Var.t -> ('ty, 'lcl) t

    val adjustment : ('ty, 'lcl) Adjustment.t -> ('ty, 'lcl) t

  end

  val left_residual : ('ty, 'lcl) Factor.t -> ('ty, 'lcl) t -> ('ty, 'lcl) t

  val compose : ('ty, 'lcl) Factor.t -> ('ty, 'lcl) t -> ('ty, 'lcl) t

  module Submode_error : sig

    type t =
      | Escaping 
      | Adjusted of Adjustment.Equal_error.t

    val swap_position : t -> t

  end

  val submode :
    equal:(string -> int -> 'ty1 -> 'ty2 -> unit)
    -> sublocal:('lcl1 -> 'lcl2 -> unit)
    -> escaping:'lcl2
    -> ('ty1, 'lcl1) t
    -> ('ty2, 'lcl2) t
    -> (unit, Submode_error.t) Result.t

  val iter : ('ty -> unit) -> 'ty t -> unit

  val fold : ('a -> 'ty -> 'a) -> 'a -> 'ty t -> 'a

  val copy : ('ty -> 'ty) -> 'ty t -> 'ty t

  val copy_fold :
    ('a -> 'ty -> 'a * 'ty) -> 'a -> 'ty t -> 'a * 'ty t

end
