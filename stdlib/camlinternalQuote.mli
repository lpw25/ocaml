
(* The representation of possibly open expressions *)
type expr_repr

(* The representation of closed expressions *)
type closed_expr_repr

val open_expr : closed_expr_repr -> expr_repr

val loc_none : Location.t

val sample_lid  : Longident.t Location.loc

val sample_name : string Location.loc

val sample_pat_list : Parsetree.pattern list

val sample_pats_names : Parsetree.pattern list * string Location.loc list

val assert_   : Location.t -> expr_repr -> expr_repr

val lazy_     : Location.t -> expr_repr -> expr_repr

val quote    : Location.t -> expr_repr -> expr_repr

val escape   : Location.t -> expr_repr -> expr_repr

val sequence : Location.t -> expr_repr -> expr_repr -> expr_repr

val while_    : Location.t -> expr_repr -> expr_repr -> expr_repr

val apply : Location.t -> (Asttypes.label * expr_repr) array -> expr_repr

val tuple : Location.t -> expr_repr array -> expr_repr

val array : Location.t -> expr_repr array -> expr_repr

val if_ :
  Location.t -> expr_repr -> expr_repr -> expr_repr option -> expr_repr

val construct  :
  Location.t -> Longident.t Location.loc -> expr_repr array -> expr_repr

val record :
  Location.t -> (Longident.t Location.loc * expr_repr) array ->
  expr_repr option -> expr_repr

val field :
  Location.t -> expr_repr -> Longident.t Location.loc -> expr_repr

val setfield :
  Location.t -> expr_repr -> Longident.t Location.loc -> expr_repr -> expr_repr

val variant  : Location.t -> string -> expr_repr option -> expr_repr

val send     : Location.t -> expr_repr -> string -> expr_repr

val open_ :
  Location.t -> Longident.t Location.loc -> Asttypes.override_flag ->
  expr_repr -> expr_repr

val fun_nonbinding :
  Location.t -> string -> Parsetree.pattern list ->
  (expr_repr option * expr_repr) array -> expr_repr

val fun_simple :
  Location.t -> string -> string Location.loc ->
  (expr_repr -> expr_repr) -> expr_repr

val for_ :
  Location.t -> string Location.loc -> expr_repr -> expr_repr ->
  bool -> (expr_repr -> expr_repr) -> expr_repr

val let_simple_nonrec :
  Location.t -> string Location.loc -> expr_repr ->
    (expr_repr -> expr_repr) -> expr_repr

val fun_ :
  Location.t -> string ->
  (Parsetree.pattern list * string Location.loc list) ->
  (expr_repr array -> (expr_repr option * expr_repr) array) -> expr_repr

val let_ :
  Location.t -> bool ->
  (Parsetree.pattern list * string Location.loc list) ->
  (expr_repr array -> (expr_repr option * expr_repr) array) -> expr_repr

val match_ :
  Location.t -> (Parsetree.pattern list * string Location.loc list) ->
  expr_repr -> int ->
  (expr_repr array -> (expr_repr option * expr_repr) array) -> expr_repr

val try_ :
  Location.t -> (Parsetree.pattern list * string Location.loc list) ->
  expr_repr ->
  (expr_repr array -> (expr_repr option * expr_repr) array) -> expr_repr

(* TODO: remove this when no longer needed. *)
val dyn_fail  : unit -> 'a
