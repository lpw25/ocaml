
val hash_name : string -> int

module Position : sig

  type t =
    { name : string;
      index : int; }
  (** A shiftable effect name. The [index] must be a natural number. *)

end

module Binding : sig

  type 'ty t =
    | Unused
    | Handled of 'ty
    | Unknown

end

module Context : sig

  type 'ty t
  (** An effect context describes a stack of handlers.

      Equivalently, they are functions that map {!Position.t}s
      to {!Binding.t}s. *)

  val mk : (string * 'ty Binding.t) list -> 'ty t
  (** [mk alist] represents a stack of handlers which binds the listed
      names to the given types. Names not listed, or mapped to [Unknown]
      are unspecified: they may or may not have handlers in the stack.

      For example, [mk ["n", Handled ty]] represents a stack of handlers
      where the top-most handler named [n] has effect type [ty]. Whilst,
      [mk ["n", Unknown; "n", Handled ty]] represents a stack of handlers
      where the top-most handler named [n] has an unknown type and the
      second handler named [n] has effect type [ty]. *)

  val empty : 'ty t
  (** [empty] is [mk []] *)

  module Desc : sig

    type 'ty t = (string * 'ty Binding.t) list

  end

  val desc : 'ty t -> 'ty Desc.t

end

module Handler : sig

  type 'ty t = (string * 'ty) list
  (** An individual handler can be represented by the list of effects it
      handles and the types at which it handles them. *)

end

module Mapping : sig

  type ('ty, 'pos) t =
    | Rename of 'pos
    | Bind of 'ty Binding.t

  val unused : ('ty, 'pos) t
  (** [unused] is [Bind Unused] *)

  val unknown : ('ty, 'pos) t
  (** [unknown] is [Bind Unknown] *)

end

module Adjustment : sig

  type 'ty t
  (** An effect adjustment describes a transformation of a stack of
      handlers.

      Equivalently, they are functions that map {!Position.t}s
      to {!Mapping.t}s. *)

  module Parse : sig

    module Outer : sig

      type t =
        | Rename of string
        | Unknown

    end

    module Inner : sig

      type 'ty t = ('ty, string) Mapping.t

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
    -> ('ty t, Parse.Error.t) Result.t
  (** [parse ~outer ~inner] constructs an adjustment where [outer] is
      essentially a pattern matching the input stack of handlers and
      [inner] is a description of the resulting stack of handlers.
      Parts of the original stack of handlers that are not matched pass
      through to the resulting stack of handlers.

      For example,
      {[
          parse
            ~outer:["A", Unused; "B", Rename "x"]
            ~inner:["C", Rename "x"; "D", Bind (Handled "t")]
      ]}
      represents a transformation that removes the top-most "A" handler,
      renames the top-most "B" handler to a "C" handler, and adds a new
      top-most "D" handler. *)

  val handler : 'ty Handler.t -> 'ty t
  (** [handler ["n1", t1; ...]] is [parse ~outer:[]
      ~inner:["n1", Bind t1; ...]]. Since this doesn't do any renaming
      there is no possibility of a parse error. *)

  val identity : 'ty t
  (** [identity] is [parse ~outer:[] ~inner:[]]. Since this doesn't do any
      renaming there is no possibility of a parse error. *)

  module Desc : sig

    module Outer : sig

      type t =
        | Rename of string
        | Unknown

    end

    module Inner : sig

      type ('ty, 'pos) t = ('ty, 'pos) Mapping.t =
        | Rename of 'pos
        | Bind of 'ty Binding.t

    end

    type ('ty, 'pos) t =
      { outer : (string * Outer.t) list;
        inner : (string * ('ty, 'pos) Inner.t) list; }

  end

  val desc : 'ty t -> ('ty, string) Desc.t

end

module Renaming : sig

  type empty = |

  type t = empty Adjustment.t
  (** A renaming is an adjustment that does not bind any effects *)

  val to_adjustment : t -> 'ty Adjustment.t

  val identity : t
  (** [identity] is [Adjustment.identity] *)

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
  (** [unused] is an effect mode that represents that a value will not
      be used at all. *)

  val unadjusted : 'lcl -> ('ty, 'lcl) t
  (** [unadjusted l] is [relative Adjustment.identity] if [l] is the
      local mode and [absolute Context.empty] if [l] is the global mode. *)

  val is_usable : ('ty, 'lcl) t -> bool
  (** [is_usable t] is [false] if [t] is [unused] or if it contains any
      [Unused] bindings, and [true] otherwise. *)

  module Apply : sig

    module Error : sig

      type t =
        | Different_effect_names of string * string

    end

  end

  val apply :
    ('ty, 'lcl) t
    -> Position.t
    -> (('ty, Position.t) Mapping.t, Apply.Error.t) result
  (** [apply t pos] treats the mode as a function from {!Position.t}
      to {!Mapping.t}.

      - If [t] and [pos] use different effect names with the same hash then
        it will return [Error (Differet_effect_names(n1, n2))].
      - If [t] is [absolute ctx] then it will return [Ok (Bind b)] where
        [b] is the binding to which [ctx] maps [pos].
      - If [t] is [relative adj] then it will return [Ok m] where
        [m] is the mapping to which [adj] maps [pos].
      - If [t] is [unused] then it will return [Ok (Bind Unused)]. *)

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

  val identity : ('ty, 'lcl) t
  (** An alias for [local]: it behaves as the identity with respect to
      [compose]. *)

  val is_adjusted : ('ty, 'lcl) t -> bool
  (** [is_adjusted t] is [false] if [t] is equal to [unadjusted l] for
      some [l] and is [true] otherwise. *)

  val is_relative : ('ty, 'lcl) t -> bool
  (** [is_relative t] is [true] if [t] is equal to [relative adj] for
      some [adj] and is [false] otherwise. *)

  module Equal : sig

    module Error : sig

      type t =
        | Not_absolute of Misc.position
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
        | Difference of Equal.Error.t

      val swap_position : t -> t

    end

  end

  val less_than :
    type_greater_than:('a -> string -> int -> 'ty1 -> 'ty2 -> 'a)
    -> local_greater_than:('lcl1 -> 'lcl2 -> unit)
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

      [less_than ~type_greater_than ~local_greater_than ~local_top t1
      t2] checks that [t1] is less than [t2] according to this partial
      order. Using [type_greater_than] and [local_greater_than] for the
      ordering on types and localities respectively.

      An accumulator is threaded through all calls to [type_equal] and
      [local_equal], allowing them to collect constraints if needed.

      Note that an effect mode on a context or arrow is contravariant for
      this ordering. *)

  val left_residual :
    type_less_than:(string -> int -> 'ty1 -> 'ty2 -> unit)
    -> type_join:(string -> int -> 'ty1 -> string -> int -> 'ty1 -> 'ty1)
    -> ('ty1, 'lcl1) t
    -> ('ty2, 'lcl2) t
    -> (('ty1, 'lcl) t, Less_than.Error.t) Result.t
  (** [left_residual ~type_less_than ~type_join t1 t2] is the greatest effect
      mode [t] such that
      {[
        less_than ~type_greater_than ~local_greater_than ~local_top
          (compose t t2) t1
      ]}
      holds.

      If that equation holds for any [t] then the left residual exists
      and is unique. *)

  val build_less_than :
    local_newvar_below:('lcl -> 'lcl * bool)
    -> ('ty, 'lcl) t
    -> ('ty, 'lcl) t * bool
  (** [build_less_than ~local_newvar_below t] is some mode less than
      [t]. It is created by replacing localities with fresh locality
      variables bounded below by the original locality.

      It also returns a bool that is the logical or of the bools
      returned by calls to [local_newvar_below]. This can be used to
      determine if the new mode is equal to [t]. *)

  val build_greater_than :
    local_newvar_above:('lcl -> 'lcl * bool)
    -> ('ty, 'lcl) t
    -> ('ty, 'lcl) t * bool
  (** [build_greater_than ~local_newvar_above t] is some mode greater
      than [t]. It is created by replacing localities with fresh
      locality variables bounded above by the original locality.

      It also returns a bool that is the logical or of the bools
      returned by calls to [local_newvar_above]. This can be used to
      determine if the new mode is equal to [t].*)

  val iter_types : ('ty -> unit) -> ('ty, 'lcl) t -> unit

  val fold_types : ('a -> 'ty -> 'a) -> 'a -> ('ty, 'lcl) t -> 'a

  val copy_types : ('ty -> 'ty) -> ('ty, 'lcl) t -> ('ty, 'lcl) t

  val copy_fold_types :
    ('a -> 'ty -> 'a * 'ty) -> 'a -> ('ty, 'lcl) t -> 'a * ('ty, 'lcl) t

  module Desc : sig

    module Direction : sig

      type t =
        | Positive
        | Negative

    end

    type 'ty t =
      | Absolute of 'ty Context.Desc.t
      | Relative of ('ty, string) Adjustment.Desc.t
      | Unused

    val map : ('a -> 'b) -> 'a t -> 'b t

    val is_global : 'ty t -> bool

  end

  val desc : Desc.Direction.t -> ('ty, 'lcl) t -> 'ty Desc.t

end

module rec Factor : sig

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
        determined.

        These are used for closures with no expected effect mode.

        Variables always stand for a mode strictly greater than
        [unused]. This allows us to ensure that all function expressions
        are potentially callable. *)

    val create : unit -> ('ty, 'lcl) t
    (** [create ()] makes a fresh variable factor *)

    val consume : ('ty, 'lcl) t -> ('ty, 'lcl) Mode.t
    (** [consumes t] returns the value of [t] and marks it as
        consumed. Any further attempts to constrain it will fail with an
        exception *)

  end

  module Mode : sig

    type ('ty, 'lcl) t
    (** A mode factor stands for a specific mode.

        These are used for closures with an expected effect mode. *)

    val create : ('ty, 'lcl) Mode.t -> ('ty, 'lcl) t
    (** [create m] makes a fresh mode factor for mode [m] *)

  end

  module Renaming : sig

    type ('ty, 'lcl) t
    (** A mode factor stands for a specific renaming. These factors
        support the [Mode.compose_left] operation.

        These are used for runtime renamings. *)

    val create : Renaming.t -> ('ty, 'lcl) t
    (** [create r] makes a fresh renaming factor for renaming [r] *)

  end

  module Handler : sig

    type ('ty, 'lcl) t
    (** A mode factor stands for a specific handler. These factors
        support the [Mode.weak_compose_left] operation.

        These are used for handlers. *)

    val create : 'ty Handler.t -> ('ty, 'lcl) t
    (** [create h] makes a fresh handler factor for handler [h] *)

  end

  val var : ('ty, 'lcl) Var.t -> ('ty, 'lcl) t

  val mode : ('ty, 'lcl) Mode.t -> ('ty, 'lcl) t

  val renaming : ('ty, 'lcl) Renaming.t -> ('ty, 'lcl) t

  val handler : ('ty, 'lcl) Handler.t -> ('ty, 'lcl) t

end

and Upper_bound : sig

  type ('ty, 'lcl) t
  
  val mode : ('ty, 'lcl) Mode.t -> ('ty, 'lcl) t

  val absolute : 'ty Context.t -> ('ty, 'lcl) t
  (** [absolute ctx] is [mode (Mode.absolute ctx)]. *)

  val relative : 'ty Adjustment.t -> ('ty, 'lcl) t
  (** [relative adj] is [mode (Mode.relative adj)]. *)

  val unused : ('ty, 'lcl) t
  (** [unused] is [mode Mode.unused]. *)

  val unadjusted : 'lcl -> ('ty, 'lcl) t
  (** [unadjusted l] is [mode (Mode.unadjusted l)]. *)

  val global : ('ty, 'lcl) t
  (** [global] is [mode Mode.global] *)

  val local : ('ty, 'lcl) t
  (** [local] is [mode Mode.local] *)

  val meet :
    join_local:('lcl -> 'lcl -> 'lcl)
    -> top_local:'lcl
    -> ('ty, 'lcl) t
    -> ('ty, 'lcl) t
    -> ('ty, 'lcl) t
  (** A value at effect mode [meet ~join_local ~local t1 t2] will be
      used only under adjustments permitted by both [t1] and [t2]. *)

  module Less_than = Mode.Less_than

  val less_than :
    type_greater_than:('a -> string -> int -> 'ty1 -> 'ty2 -> 'a)
    -> local_greater_than:('lcl1 -> 'lcl2 -> unit)
    -> local_top:'lcl2
    -> ('ty1, 'lcl1) Env.t
    -> ('ty1, 'lcl1) Lower_bound.t
    -> ('ty2, 'lcl2) t
    -> (unit, Less_than.Error.t) Result.t
  (** [less_than ~type_greater_than ~local_greater_than ~local_top env
      l1 t2] lifts {!Mode.less_than} to work on a lower bound and an
      upper bound, and extends it so that absolute modes are less than
      equivalent relative modes, based on [env] describing the current
      stack of handlers.*)

  val compose_right : ('ty, 'lcl) t -> ('ty, 'lcl) Mode.t -> ('ty, 'lcl) t
  (** [compose_right t m] lifts {!Mode.compose} to composing a mode to
      the right of an upper bound.

      If a value needs effect mode [t] and we project out a field with
      effect mode [m] then the resulting value needs effect mode
      [compose_right t m]. *)

  val left_residual :
    type_less_than:(string -> int -> 'ty1 -> 'ty2 -> unit)
    -> type_join:(string -> int -> 'ty1 -> string -> int -> 'ty1 -> 'ty1)
    -> ('ty1, 'lcl) t
    -> ('ty2, 'lcl) Mode.t
    -> ('ty1, 'lcl) t
  (** [left_residual ~type_sub ~type_meet t1 m2] is the greatest effect
      mode [t] such that, for all [m]:
      {[
        less_than ~type_greater_than ~local_greater_than ~local_top
          (Lower_bound.mode m) t
      ]}
      is equivalent to:
      {[
        less_than ~type_greater_than ~local_greater_than ~local_top
          (Lower_bound.mode (Mode.compose m m2)) t1
      ]}

      If a value needs effect mode [t1] and is being written into a
      field with effect mode [m2] then the resulting record needs effect
      mode [left_residual ~type_less_than ~type_join t1 m2]. *)

  val compose_left :
    ('ty, 'lcl) Factor.Renaming.t -> ('ty, 'lcl) t -> ('ty, 'lcl) t
  (** [compose_left f t] lifts {!Mode.compose} to composing a factor to
      the left of an upper bound.

      If a value needs effect mode [t] and it escapes from a runtime
      renaming with effect mode factor [f] then it now needs effect mode
      [compose_left f t]. *)

  val weak_compose_left :
    ('ty, 'lcl) Factor.Handler.t -> ('ty, 'lcl) t -> ('ty, 'lcl) t
  (** [weak_compose_left f t] is a weaker version of
      [compose_left]. Rather that composing with [f] on the left we take
      a right-residual with the right inverse of [f]. Obviously this can
      only work for factors which have a right inverse, and we restrict
      it to handler factors.

      If a value needs effect mode [t] and it escapes from a handler
      renaming with effect mode factor [f] then it now needs effect mode
      [weak_compose_left f t]. By using the weaker form of composition
      we ensure that [local] values are genuinely executed using the
      current set of handlers: otherwise a local value could leave the
      current handler and enter a different handler with the same
      type. (Allowing that would also cause principality issues related
      to [unadjusted] modes). *)

  val right_residual : ('ty, 'lcl) Factor.t -> ('ty, 'lcl) t -> ('ty, 'lcl) t
  (** [right_residual f1 t2] is the greatest effect mode [t] such that,
      for all [m]:
      {[
        less_than ~type_greater_than ~local_greater_than ~local_top
          (Lower_bound.mode m) t
      ]}
      is equivalent to:
      {[
        less_than ~type_greater_than ~local_greater_than ~local_top
          (Lower_bound.mode (Mode.compose f1 m)) t1
      ]}

      If a value in a context with effect mode [em1] is used inside a
      runtime renaming with effect mode [em2] then the value must be
      used within further adjustment [right_residual em2 em1].

      If a value in a context with effect mode [em1] is used within a
      closure that is used within [em2] then the value must be used
      within the function at [right_residual em2 em1]. *)

  module Usable_absolute : sig

    module Error : sig

      type t =
        | Unused
        | Unused_binding of Position.t
        | Relative

    end

  end

  val usable_absolute :
    ('ty, 'lcl) t -> (('ty, 'lcl) Mode.t, Usable_absolute.Error.t) result

  val filter_types : ('ty -> bool) -> ('ty, 'lcl) t -> ('ty, 'lcl) t
  (** [filter_types f t] is the largest upper bound less than [t] which
      does not contain any types for which [f] returns [false]. This
      assumes that the ordering on types is the discrete ordering. *)

  val zap_to_mode : ('ty, 'lcl) t -> ('ty, 'lcl) t
  (** Try to make the upper bound equivalent to a single mode to improve
      printing.  This may force types to be equal or localities to be
      global. *)

  module Desc : sig

    type 'ty t =
      | Absolute of 'ty list Context.Desc.t
      | Relative of ('ty list, string list) Adjustment.Desc.t
      | Unused

    val of_mode : 'ty Mode.Desc.t -> 'ty t

    val map : ('a -> 'b) -> 'a t -> 'b t

    val is_global : 'ty t -> bool

  end

  val desc : ('ty, 'lcl) t -> 'ty Desc.t

  val iter_types : ('ty -> unit) -> ('ty, 'lcl) t -> unit

  val fold_types : ('a -> 'ty -> 'a) -> 'a -> ('ty, 'lcl) t -> 'a

  val copy_types : ('ty -> 'ty) -> ('ty, 'lcl) t -> ('ty, 'lcl) t

  val copy_fold_types :
    ('a -> 'ty -> 'a * 'ty) -> 'a -> ('ty, 'lcl) t -> 'a * ('ty, 'lcl) t

end

and Lower_bound : sig

  type ('ty, 'lcl) t

  module Var : sig

    type ('ty, 'lcl) t

    val create : 'lcl -> ('ty, 'lcl) t
    (** [create l] creates a fresh variable with [Mode.unadjusted l] as
        a lower bound. *)

    val bound : ('ty, 'lcl) t -> 'lcl
    (** [bound (create l)] is [l]. *)

    val consume : ('ty, 'lcl) t -> ('ty, 'lcl) Upper_bound.t
    (** [consumes t] returns the value of [t] and marks it as
        consumed. Any further attempts to constrain it will fail with an
        exception *)

  end

  val var : ('ty, 'lcl) Var.t -> ('ty, 'lcl) t

  val mode : ('ty, 'lcl) Mode.t -> ('ty, 'lcl) t

  val unused : ('ty, 'lcl) t
  (** [unused] is [mode Mode.unused]. *)

  val global : ('ty, 'lcl) t
  (** [global] is [mode Mode.global]. *)

  val unadjusted : 'lcl -> ('ty, 'lcl) t
  (** [unadjusted l] is [mode (Mode.unadjusted l)]. *)

  val compose_right : ('ty, 'lcl) t -> ('ty, 'lcl) Mode.t -> ('ty, 'lcl) t
  (** [compose_right t m] lifts {!Mode.compose} to composing a mode to
      the right of a lower bound. It is the left adjoint of
      {!Upper_bound.left_residual}. *)

  val weak_uncompose_left :
    ('ty, 'lcl) Factor.Handler.t -> ('ty, 'lcl) t -> ('ty, 'lcl) t
  (** [weak_uncompose_left f] is the left adjoint of
      {!Upper_bound.weak_compose_left f}, so that:
      {[
        Upper_bound.less_than ~type_greater_than ~local_greater_than ~local_top
          (weak_uncompose_left f t) u
      ]}
      is equivalent to:
      {[
        Upper_bound.less_than ~type_greater_than ~local_greater_than ~local_top
          t (Upper_bound.weak_compose_left f u)
      ]}.
      It works by composing the right inverse of [f] on the left of
      [t]. *)

  val uncompose_left :
    ('ty, 'lcl) Factor.Renaming.t -> ('ty, 'lcl) t -> ('ty, 'lcl) t
  (** [uncompose_left f] is the left adjoint of
      {!Upper_bound.compose_left f}, so that:
      {[
        Upper_bound.less_than ~type_greater_than ~local_greater_than ~local_top
          (uncompose_left f t) u
      ]}
      is equivalent to:
      {[
        Upper_bound.less_than ~type_greater_than ~local_greater_than ~local_top
          t (Upper_bound.compose_left f u)
      ]}. *)

  val desc : ('ty, 'lcl) t -> 'ty Mode.Desc.t

  val iter_types : ('ty -> unit) -> ('ty, 'lcl) t -> unit

  val fold_types : ('a -> 'ty -> 'a) -> 'a -> ('ty, 'lcl) t -> 'a

end

and Env : sig

  type ('ty, 'lcl) t

  val empty : ('ty, 'lcl) t

  val extend : ('ty, 'lcl) t -> ('ty, 'lcl) Factor.t -> ('ty, 'lcl) t

end

module Inferred_upper_bound : sig

  type ('ty, 'lcl) t
  (** Due to the weird order we type let definitions, we sometimes need
      to construct an upper bound based off of a lower bound variable
      that has not yet been constrained. This type represents such an
      upper bound and supports the subset of upper bound operations
      needed for them. *)
  
  val var : ('ty, 'lcl) Lower_bound.Var.t -> ('ty, 'lcl) t
  (** [var t] represents the upper bound that will be produced by [t]
      when it is eventually consumed. *)

  val known : ('ty, 'lcl) Upper_bound.t -> ('ty, 'lcl) t
  (** [known t] represents the upper bound [t] directly. *)

  val mode : ('ty, 'lcl) Mode.t -> ('ty, 'lcl) t
  (** [mode t] is [known (Upper_bound.mode t]. *)

  val global : ('ty, 'lcl) t
  (** [global] is [mode Mode.global]. *)

  val unused : ('ty, 'lcl) t
  (** [unused] is [mode Mode.unused]. *)

  val top : ('ty, 'lcl) t
  (** [top] is the top of the lattice, which is [global]. *)

  val bottom : ('ty, 'lcl) t
  (** [bottom] is the bottom of the lattice, which is [unused]. *)

  val compose_right : ('ty, 'lcl) t -> ('ty, 'lcl) Mode.t -> ('ty, 'lcl) t
  (** [compose_right t m] lifts {!Upper_bound.compose_right} to inferred
      upper bounds. *)

  val meet : ('ty, 'lcl) t -> ('ty, 'lcl) t -> ('ty, 'lcl) t
  (** [meet t t] lifts {!Upper_bound.meet} to inferred upper bounds. *)

  val filter_types : ('ty -> bool) -> ('ty, 'lcl) t -> ('ty, 'lcl) t
  (** [filter_types f t] lifts {!Upper_bound.filter_types} to inferred
      upper bounds. *)

  val approx :
    join_local:('lcl -> 'lcl -> 'lcl)
    -> top_local:'lcl
    -> ('ty, 'lcl) t
    -> ('ty, 'lcl) Upper_bound.t
  (** [bound ~join_local ~top_local t] produces a conservative estimate
      of the bound by using {!Lower_bound.Var.bound} as the value of any
      underlying variables, using [join_local] and [top_local] to
      calculate any meets. *)

  val consume :
    join_local:('lcl -> 'lcl -> 'lcl)
    -> top_local:'lcl
    -> ('ty, 'lcl) t
    -> ('ty, 'lcl) Upper_bound.t
  (** [consume ~join_local ~top_local t] calls {!Lower_bound.Var.consume} on
      any underlying variables and calculates the resulting concrete upper
      bound, using [join_local] and [top_local] to calculate any meets. *)

  val iter_types : ('ty -> unit) -> ('ty, 'lcl) t -> unit

  val copy_types : ('ty -> 'ty) -> ('ty, 'lcl) t -> ('ty, 'lcl) t

end
