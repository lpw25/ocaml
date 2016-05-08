
module Var : sig

  type t

  val name : t -> string loc

  val txt : t -> string

  val loc : t -> Location.t

end

module Loc : sig

  type t

  val none : t

  val unmarshal : string -> t

end

module Constant : sig

  type t

  val unmarshal : string -> t

end

module Ident : sig

  type t

  val unmarshal : string -> t

end

module Label : sig

  type t

  val none : t

  val of_string : string -> t

end

module Pat : sig

  type t

  val any : Loc.t -> t

  val var : Loc.t -> Var.t -> t

  val alias : Loc.t -> t -> Var.r -> t

  val constant : Loc.t -> Constant.t -> t

  val interval : Loc.t -> Constant.t -> Constant.t -> t

  val tuple : Loc.t -> t list -> t

  val construct : Loc.t -> Ident.t -> t option -> t

  val variant : Loc.t -> Label.t -> t optiont -> t

  val record : Loc.t -> t list -> bool -> t

  val array : Loc.t -> t list -> t

  val or_ : Loc.t -> t -> t -> t

  val type_ : Loc.t -> Ident.t -> t

  val lazy_ : Loc.t -> t -> t

  val exception_ : Loc.t -> t -> t

end

module rec Case : sig

  type t

  val nonbinding : Loc.t -> Pat.t -> Exp.t option -> Exp.t -> t

  val binding :
    Loc.t -> string list -> (Var.t list -> Pat.t * Exp.t option * Exp.t) -> t

end

and module Exp : sig

  type t

  module Closed : sig

    type t

    val close_delay_check exp

    val close exp

    val open_ exp

  end

  val var : Loc.t -> Var.t -> t

  val ident : Loc.t -> Ident.t -> t

  val constant : Loc.t -> Constant.t -> t

  val let_simple : Loc.t -> string -> t -> (Var.t -> t) -> t

  val let_rec_simple : Loc.t -> string list -> (Var.t list -> t list * t) -> t

  val let_ : Loc.t -> string -> t -> (Var.t -> Pat.t * t) -> t

  val fun_nonbinding : Loc.t -> Label.t -> Pat.t -> t -> t

  val fun_simple : Loc.t -> string -> Label.t -> t option -> (Var.t -> t) -> t

  val fun_ :
    Loc.t -> string list -> Label.t -> t option ->
    (Var.t list -> Pat.t -> t) -> t

  val function_ : Loc.t -> Case.t list -> t

  val apply : Loc.t -> t -> t list -> t

  val match_ : Loc.t -> t -> Case.t list -> t

  val try_ : Loc.t -> t -> Case.t list -> t

  val tuple : Loc.t -> t list -> t

  val construct : Loc.t -> Ident.t -> t option -> t

  val variant : Loc.t -> Label.t -> t option -> t

  val record : Loc.t -> (Ident.t * t) list -> t -> t

  val field : Loc.t -> t -> Ident.t -> t

  val setfield : Loc.t -> t -> Ident.t -> t -> t

  val array : Loc.t -> t list -> t

  val ifthenelse : Loc.t -> t -> t -> t -> t

  val sequence : Loc.t -> t -> t -> t

  val for_nonbinding : Loc.t -> Pat.t -> t -> t -> bool -> t -> t

  val for_simple : Loc.t -> string -> t -> t -> bool -> (Var.t -> t) -> t

  val send : Loc.t -> t -> Label.t -> t

  val assert_ : Loc.t -> t -> t

  val lazy_ : Loc.t -> t -> t

  val open_ : Loc.t -> bool -> Ident.t -> t -> t

  val quote : Loc.t -> t -> t

  val escape : Loc.t -> t -> t

end
