
val hash_name : string -> int

module Position : sig

  type t =
    { name : string;
      index : int; }
  (** A (shifted) effect name. The [index] must be a natural number. *)

end

module Binding : sig

  type 'ty t =
    | Handled of 'ty
    | Unknown

end

module Context : sig

  type 'ty t
  (** An effect context describes a stack of handlers.

      Equivalently, they are functions that map {!Position.t}s
      to {!Binding.t}s. *)

  val mk : (string * 'ty option) list -> 'ty t
  (** [mk alist] represents a stack of handlers which binds the listed
      names to the given types. Names not listed, or mapped to [None]
      are unspecified: they may or may not have handlers in the stack.

      For example, [mk ["n", Some ty]] represents a stack of handlers
      where the top-most handler named [n] has effect type [ty]. Whilst,
      [mk ["n", None; "n", Some ty]] represents a stack of handlers
      where the top-most handler named [n] has an unknown type and the
      second handler named [n] has effect type [ty]. *)

  val empty : 'ty t
  (** [empty] is [mk []] *)

end

module Mapping : sig

  type 'ty t =
    | Rename of Position.t
    | Bind of 'ty Binding.t

end

module Adjustment : sig

  type 'ty t
  (** An effect adjustment describes a transformation of a stack of
      handlers.

      Equivalently, they are functions that map {!Position.t}s
      to {!Mapping.t}s. *)

  module Parse : sig

    module Outer : sig

      type t = string option

    end

    module Inner : sig

      type 'ty t =
        | Rename of string
        | Bind of 'ty option

    end

    module Error : sig

      type t =
        | Multiple_outer_uses of string
        | No_outer_uses of string
        | No_inner_uses of string

      val print : Format.formatter -> t -> unit

    end

  end

  val parse :
    outer:(string * Parse.Outer.t) list
    -> inner:(string * 'ty Parse.Inner.t) list
    -> ('ty t, Parse_error.t) Result.t
  (** [parse ~outer ~inner] constructs an adjustment where [outer] is
      essentially a pattern matching the input stack of handlers and
      [inner] is a description of the resulting stack of handlers.
      Parts of the original stack of handlers that are not matched pass
      through to the resulting stack of handlers.

      For example,
      {[
          parse
            ~outer:["A", None; "B", Some "x"]
            ~inner:["C", Rename "x"; "D", Bind (Some "t")]
      ]}
      represents a transformation that removes the top-most "A" handler, renames the
      top-most "B" handler to a "C" handler, and adds a new top-most "D" handler. *)

  val handler : (string * 'ty) list -> 'ty t
  (** [handler ["n1", t1; ...]] is [parse ~outer:[]
      ~inner:["n1", Bind t1; ...]]. Since this doesn't do any renaming
      there is no possibility of a parse error. *)

  val identity : 'ty t
  (** [identity] is [parse ~outer:[] ~inner:[]]. Since this doesn't do any
      renaming there is no possibility of a parse error. *)
end

module Mode : sig

  type ('ty, 'lcl) t
  (** An effect mode [em] describes under what effect handlers a value
      will be used.

      Note that here "used" refers to running potentially effectful
      computations: operations that are not computations,
      e.g. projecting a record field or matching on a variant
      constructor, do not count as uses. *)

  val absolute : 'ty Context.t -> ('ty, 'lcl) t
  (** Values at effect mode [absolute ctxt] can be used with any stack
      of effect handlers that matches the context [ctxt]. *)

  val relative : 'ty Adjustment.t -> ('ty, 'lcl) t
  (** Values at effect mode [relative adj] can be used after the
      adjustment [adj] has been made to the current stack of effect
      handlers. *)

  val unused : ('ty, 'lcl) t
  (** [unused] is a special relative effect mode, which represents that
      a value will not be used at all. *)

  val unadjusted : 'lcl -> ('ty, 'lcl) t
  (** [unadjusted l] is [relative Adjustment.identity] if [l] is the
      local mode and [absolute Context.empty] if [l] is the global mode. *)

  val is_unused : ('ty, 'lcl) t -> bool
  (** [is_unused t] is [true] if [t] is [unused] and [false] otherwise. *)

  module Apply : sig

    module Error : sig

      type t =
        | Unused
        | Different_effect_names of string * string

    end

  end

  val apply : ('ty, 'lcl) t -> Position.t -> ('ty Mapping.t, Apply.Error.t) result
  (** [apply t pos] treats the mode as a function from {!Position.t}
      to {!Mapping.t}.

      - If [t] and [pos] use different effect names with the same hash then
        it will return [Error (Differet_effect_names(n1, n2))].
      - If [t] is [absolute ctx] then it will return [Ok (Bind b)] where
        [b] is the binding to which [ctx] maps [pos].
      - If [t] is [relative adj] then it will return [Ok m] where
        [m] is the mapping to which [adj] maps [pos].
      - If [t] is [unused] then it will return [Error Unused]. *)

  val compose : ('ty, 'lcl) t -> ('ty, 'lcl) t -> ('ty, 'lcl) t
  (** [compose e1 e2] is the effect mode such that [apply (compose e1 e2) pos]
      is equal to [apply e1 (apply e2 pos)].

      This operation isn't quite total due to the possibility of hash
      collisions between different effect names. *)

  val global : ('ty, 'lcl) t
  (** Equivalent to [absolute Context.empty]. A value at effect mode
      [global] may be used in any effect context. Note that all absolute
      effect modes can be constructed by composing [global] with a
      relative effect mode. *)

  val local : ('ty, 'lcl) t
  (** Equivalent to [relative Identity.empty]. A value at effect mode
      [local] may only be used in the current effect context. *)

  module Equal : sig

    module Error : sig

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

  end

  val equal :
    type_equal:('a -> string -> int -> 'ty1 -> 'ty2 -> 'a)
    -> local_equal:('a -> 'lcl1 -> 'lcl2 -> 'a)
    -> 'a
    -> ('ty1, 'lcl1) t
    -> ('ty2, 'lcl2) t
    -> ('a, Equal.Error.t) Result.t
  (** [equal ~type_equal ~local_equal acc t1 t2] checks that [apply t1
      pos] equals [apply t2 pos] for all valid [pos], with [type_equal]
      and [local_equal] used to check equality of types and
      localities.

      An accumulator is threaded through all calls to [type_equal] and
      [local_equal], allowing them to collect constraints if needed. *)

  module Less_than : sig

    module Error : sig

      type t =
        | Escaping 
        | Adjusted of Adjustment.Equal_error.t

      val swap_position : t -> t

    end

  end

  val less_than :
    type_sub:('a -> string -> int -> 'ty1 -> 'ty2 -> 'a)
    -> local_sub:('lcl1 -> 'lcl2 -> unit)
    -> local_top:'lcl2
    -> 'a
    -> ('ty1, 'lcl1) t
    -> ('ty2, 'lcl2) t
    -> ('a, Less_than.Error.t) Result.t
  (** Both effect contexts and effect adjustments can include [Unknown]
      bindings -- written e.g. [A : .]. There is a partial order on
      effect modes such that [[A as x/B as x]] < [[A : ./B : .]] and [[A
      : t; B : s]] < [[A : t]]. [unused] is the bottom element of this
      partial order.

      [less_than ~type_sub ~local_sub ~local_top t1 t2] checks that [t1] is less
      than [t2] according to this partial order. Using [type_sub] and [local_sub]
      for the ordering on types and localities respectively.

      An accumulator is threaded through all calls to [type_equal] and
      [local_equal], allowing them to collect constraints if needed.

      Note that an effect mode on a context or arrow is contravariant for
      this ordering. *)

  val left_residual :
    type_sub:(string -> int -> 'ty1 -> 'ty2 -> unit)
    -> type_meet:(string -> int -> 'ty1 -> string -> int -> 'ty1 -> 'ty1)
    -> ('ty1, 'lcl1) t
    -> ('ty2, 'lcl2) t
    -> (('ty1, 'lcl) t, Less_than.Error.t) Result.t
  (** [left_residual ~type_sub ~type_meet t1 t2] is the greatest effect
      mode [t] such that
      {[
        less_than ~type_sub ~local_sub ~local_top (compose t t2) t1
      ]}
      holds.

      If that equation holds for any [t] then the left residual exists
      and is unique. *)

  val iter_types : ('ty -> unit) -> 'ty t -> unit

  val fold_types : ('a -> 'ty -> 'a) -> 'a -> 'ty t -> 'a

  val copy_types : ('ty -> 'ty) -> 'ty t -> 'ty t

  val copy_fold_types :
    ('a -> 'ty -> 'a * 'ty) -> 'a -> 'ty t -> 'a * 'ty t

  module Desc : sig

    type 'ty t =
      | Absolute of 'ty Context.t
      | Relative of 'ty Adjustment.t
      | Unused

    val map : ('a -> 'b) -> 'a t -> 'b t

  end

  val desc : ('ty, 'lcl) t -> 'ty Desc.t

end

module rec Lower_bound : sig

  type ('ty, 'lcl) t

  module Var : sig

    type ('ty, 'lcl) t

    val create : unit -> ('ty, 'lcl) t

    val consume : ('ty, 'lcl) t -> ('ty, 'lcl) Upper_bound.t
    (** [consumes t] returns the value of [t] and marks it as
        consumed. Any further attempts to constrain it will fail with an
        exception *)

  end

  val var : ('ty, 'lcl) Var.t -> ('ty, 'lcl) t

  val mode : ('ty, 'lcl) Mode.t -> ('ty, 'lcl) t

end

and Factor : sig

  type ('ty, 'lcl) t
  (** Effect modes do not in general have a right residual. Instead we
      provide a restricted form: we restrict the left compose and
      right residual operations to work with a [Factor.t] type that
      behaves as a resource (i.e. each call to the [create] functions
      creates a fresh factor not equal to any other). Composition by a
      factor is not defined on the right residuals of other
      factors. Since we only need to use right residuals and
      left composition according to a stack discipline, this is
      sufficient. *)

  module Var : sig

    type ('ty, 'lcl) t
    (** A variable factor stands for a mode that is yet to be
        determined. Variables always stand for a mode strictly greater
        than [unused]. This allows us to ensure that all function
        expressions are potentially callable. *)

    val create : unit -> ('ty, 'lcl) t
    (** [create ()] makes a fresh variable factor *)

    val consume : ('ty, 'lcl) t -> ('ty, 'lcl) Mode.t
    (** [consumes t] returns the value of [t] and marks it as
        consumed. Any further attempts to constrain it will fail with an
        exception *)

  end

  module Mode : sig

    type ('ty, 'lcl) t
    (** A mode factor stands for a specific mode. *)

    val create : ('ty, 'lcl) Mode.t -> ('ty, 'lcl) t
    (** [create m] makes a fresh mode factor for mode [m] *)

  end

  val var : ('ty, 'lcl) Var.t -> ('ty, 'lcl) t

  val mode : ('ty, 'lcl) Mode.t -> ('ty, 'lcl) t

end

and Upper_bound : sig

  type ('ty, 'lcl) t
  
  val mode : ('ty, 'lcl) Mode.t -> ('ty, 'lcl) t

  val meet :
    join_local:('lcl -> 'lcl -> 'lcl)
    -> local:'lcl
    -> ('ty, 'lcl) t
    -> ('ty, 'lcl) t
    -> ('ty, 'lcl) t
  (** A value at effect mode [meet ~join_local ~local t1 t2] will be
      used only under adjustments permitted by both [t1] and [t2]. *)

  val less_than :
    type_sub:('a -> string -> int -> 'ty1 -> 'ty2 -> 'a)
    -> local_sub:('lcl1 -> 'lcl2 -> unit)
    -> local_top:'lcl2
    -> ('ty1, 'lcl1) Env.t
    -> ('ty1, 'lcl1) Lower_bound.t
    -> ('ty2, 'lcl2) t
    -> (unit, Less_than.Error.t) Result.t
  (** [less_than ~type_sub ~local_sub ~local_top env l1 t2] lifts {!Mode.less_than}
      to work on a lower bound and an upper bound, and extends it so that
      absolute modes are less than equivalent relative modes, based on [env]
      describing the current stack of handlers.*)

  val compose_right : ('ty, 'lcl) t -> ('ty, 'lcl) Mode.t -> ('ty, 'lcl) t
  (** [compose_right t m] lifts {!Mode.compose} to composing a mode to
      the right of an upper bound.

      If a value needs effect mode [t] and we project out a field with
      effect mode [m] then the resulting value needs effect mode
      [compose_right t m]. *)

  val left_residual :
    equal:(string -> int -> 'ty1 -> 'ty2 -> unit)
    -> meet:(string -> int -> 'ty1 -> string -> int -> 'ty1 -> 'ty1)
    -> ('ty1, 'lcl) t
    -> ('ty2, 'lcl) Mode.t
    -> ('ty1, 'lcl) t
  (** [left_residual ~type_sub ~type_meet t1 m2] is the greatest effect
      mode [t] such that, for all [m]:
      {[
        less_than ~type_sub ~local_sub ~local_top
          (Lower_bound.mode m) t
      ]}
      is equivalent to:
      {[
        less_than ~type_sub ~local_sub ~local_top
          (Lower_bound.mode (Mode.compose m m2)) t1
      ]}

      If a value needs effect mode [t1] and is being written into a
      field with effect mode [t2] then the resulting record needs effect
      mode [left_residual ~type_sub ~type_meet t1 t2]. *)

  val compose_left : ('ty, 'lcl) Factor.Mode.t -> ('ty, 'lcl) t -> ('ty, 'lcl) t
  (** [compose_left f t] lifts {!Mode.compose} to composing a factor to
      the left of an upper bound.

      If a value needs effect mode [t] and it escapes from a runtime
      renaming with effect mode factor [f] then it now needs effect mode
      [compose_left f t]. *)

  val right_residual : ('ty, 'lcl) Factor.t -> ('ty, 'lcl) t -> ('ty, 'lcl) t
  (** [right_residual f1 t2] is the greatest effect mode [t] such that,
      for all [m]:
      {[
        less_than ~type_sub ~local_sub ~local_top
          (Lower_bound.mode m) t
      ]}
      is equivalent to:
      {[
        less_than ~type_sub ~local_sub ~local_top
          (Lower_bound.mode (Mode.compose m m2)) t1
      ]}

      If a value in a context with effect mode [em1] is used inside a
      runtime renaming with effect mode [em2] then the value must be used within further
      adjustment [right_residual em2 em1].

      If a value in a context with effect mode [em1] is used within a
      closure that is used within [em2] then the value must be used
      within the function at [right_residual em2 em1]. *)


end

and Env : sig

  type ('ty, 'lcl) t

  val empty : ('ty, 'lcl) t

  val extend : ('ty, 'lcl) t -> ('ty, 'lcl) Factor.t -> ('ty, 'lcl) t

end
