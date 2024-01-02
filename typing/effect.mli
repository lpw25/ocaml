
module Adjustment : sig

  type 'ty t
  (** An adjustment [adj] describes how effects emitted from one context
      will be mapped onto another.  Given a value that will produce
      effects at [ctx] it will produce effects at [apply adj ctx].

      An adjustment [adj] on a function parameter type indicates that
      the function will run the computation within that adjustment. That
      means that if the argument performs effects at [ctx] then the
      function will perform effects at [apply adj ctx]

      A value in a context with adjustment [adj] must have that
      adjustment applied before it can be used.

      The ordering an adjustments is such that:

      [A as x/B as x] ≤ [A : ./B : .]

      An adjustment on a context or arrow is contravariant for this
      ordering.

      If a value in a context needing adjustment [adj1] is used inside a
      runtime adjustment of [adj2] then it must be used within further
      adjustment [adj2\adj1]: the right-residual.

      If a value in a context with adjustment [adj1] is used within a
      closure that is used within [adj2] then the value must be used
      within the function at [adj2\adj1]: the right-residual.

      If a value needs adjustment [adj1] and it escapes from a runtime
      adjustment [adj2] then it now needs adjustment [compose adj2
      adj1].

      If a value needs adjustment [adj1] and is being written into a
      field with adjustment [adj2] then the resulting record needs
      adjustment [adj1/adj2]: the left-residual.

      If a value needs adjustment [adj1] and we project out a field with
      adjustment [adj2] then the resulting value needs adjustment
      [compose adj1 adj2]. *)

  module Parse_error : sig

    type t =
      | Multiple_outer_uses of string
      | No_outer_uses of string
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
  (** The adjustment of an effect handler *)

  val identity : 'ty t
  (** The identity adjustment with no explicit transparent renamings. *)

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

  val expected_context : newvar:(unit -> 'ty) -> 'ty t -> 'ty Effect_context.t

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

    module Residual_error : sig

      type t =
        | Unexpected_bind of string * int

    end

    val left_residual :
      equal:(string -> int -> 'ty1 -> 'ty2 -> unit)
      -> meet:(string -> int -> 'ty1 -> string -> int -> 'ty1 -> 'ty1)
      -> 'ty1 t
      -> 'ty2 t
      -> ('ty1 t, Residual_error.t) Result.t

    val reify : 'ty t -> 'ty adjustment

  end

  val simplify : 'ty t -> 'ty Simplified.t

end

module Mode : sig

  type ('ty, 'lcl) t
  (** An effect mode [em] describes under what adjustments a value will be used.

      Note that here "used" refers to running potentially effectful
      computations: operations that are not computations,
      e.g. projecting a record field or matching on a variant
      constructor, do not count as uses. *)

  val escaping : ('ty, 'lcl) t
  (** A value at effect mode [escaping] will be used under unknown adjustments. *)

  val unused : ('ty, 'lcl) t
  (** A value at effect mode [unused] will not be used. *)

  val adjusted : 'ty Adjustment.t -> ('ty, 'lcl) t
  (** A value at effect mode [adjusted adj] will be used only under the
      adjustment [adj]. *)

  val local : ('ty, 'lcl) t
  (** [local] is [adjusted Adjustment.identity] *)

  val unadjusted : 'lcl -> ('ty, 'lcl) t
  (** A value at effect mode [unadjusted l] is [local] if [l] is the
      local mode and [escaping] if [l] is the global mode. *)

  val meet :
    join:('lcl -> 'lcl -> 'lcl)
    -> local:'lcl
    -> ('ty, 'lcl) t
    -> ('ty, 'lcl) t
    -> ('ty, 'lcl) t
  (** A value at effect mode [meet em1 em2] will be used only under
      adjustments permitted by both [em1] and [em2]. *)

  val left_residual :
    equal:(string -> int -> 'ty1 -> 'ty2 -> unit)
    -> meet:(string -> int -> 'ty1 -> string -> int -> 'ty1 -> 'ty1)
    -> ('ty1, 'lcl) t
    -> 'ty2 Adjustment.t
    -> ('ty1, 'lcl) t
  (** We can take left residuals with respect to adjustments. Unlike for
      adjustments this operation is total due to the existence of
      [unused]. *)

  val left_residual_not_unused :
    equal:(string -> int -> 'ty1 -> 'ty2 -> unit)
    -> meet:(string -> int -> 'ty1 -> string -> int -> 'ty1 -> 'ty1)
    -> ('ty1, 'lcl) t
    -> 'ty2 Adjustment.t
    -> (('ty1, 'lcl) t, Residual_error.t) Result.t
  (** This is the same as [left_residual], unless the resulting mode
      would be [unused], in which case it gives an error describing why
      that is the case. Raises if the original mode is already
      [unused]. *)

  val compose_right : ('ty, 'lcl) t -> 'ty Adjustment.t -> ('ty, 'lcl) t
  (** We can compose with adjustments on the right. *)
 
  module Factor : sig
    (** Effect modes do not in general have a right residual. Instead we
        provide a restricted form: we restrict the left compose and
        right residual operations to work with a [Factor.t] type that
        behaves as a resource (i.e. each call to the [create] functions
        creates a fresh factor not equal to any other). Composition by a
        factor is not defined on the right residuals of other
        factors. Since we only need to use right residuals and
        composition according to a stack discipline, this is
        sufficient. Composition by a factor is not available for factor
        variables. *)

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

  val right_residual : ('ty, 'lcl) Factor.t -> ('ty, 'lcl) t -> ('ty, 'lcl) t

  val compose_left : ('ty, 'lcl) Factor.Adjustment.t -> ('ty, 'lcl) t -> ('ty, 'lcl) t

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
    -> 'ty2 Adjustment.t
    -> (unit, Submode_error.t) Result.t

  val iter : ('ty -> unit) -> 'ty t -> unit

  val fold : ('a -> 'ty -> 'a) -> 'a -> 'ty t -> 'a

  val copy : ('ty -> 'ty) -> 'ty t -> 'ty t

  val copy_fold :
    ('a -> 'ty -> 'a * 'ty) -> 'a -> 'ty t -> 'a * 'ty t

end
