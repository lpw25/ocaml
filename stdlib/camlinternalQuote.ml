(* ------------------------------------------------------------------------ *)
(* Iterations over common structures *)

let iter_opt loc f o =
  match o with
  | None -> ()
  | Some x -> f loc x

let rec iter_list loc f l =
  match l with
  | [] -> ()
  | x :: rest -> f loc x; iter_list loc f rest

let rec iter_alist loc f l =
  match l with
  | [] -> ()
  | (_, x) :: rest -> f loc x; iter_alist loc f rest

(* ------------------------------------------------------------------------ *)
(* Accumulating maps over common structures *)

let accum_opt loc f acc o =
  match o with
  | None -> (acc, None)
  | Some x ->
      let acc, x = f loc acc x in
        (acc, Some x)

let rec accum_list loc f acc l =
  match l with
  | [] -> (acc, [])
  | x :: rest ->
      let acc, x = f loc acc x in
      let acc, rest = accum_list loc f acc rest in
        (acc, x :: rest)

let rec accum_alist loc f acc l =
  match l with
  | [] -> (acc, [])
  | (k, x) :: rest ->
      let acc, x = f loc acc x in
      let acc, rest = accum_alist loc f acc rest in
        (acc, (k, x) :: rest)

(* ------------------------------------------------------------------------ *)
(* Stack marks, a simple form of dynamic binding *)
module Stackmark : sig

  type t

  val check : t -> bool

  val region : (t -> 'a) -> 'a

end = struct

  (* A robust and truly minimalistic implementation of stack-marks.
     A stack-mark is created by 'with_stack_mark' function. Since
     the only operation on a stackmark is to test if it is valid,
     the stackmark is realized as a thunk unit -> bool.
  *)
  type t = unit -> bool           (* true if valid *)

  let check t = t ()

  (* The simple implementation of stackmark_region_fn, appropriate
     when no delimited control is used.
     The mark is a ref bool cell, containing true within
     stackmark_region_fn's dynamic region.
  *)
  let region body =
    let mark = ref true in
      try
        let r = body (fun () -> !mark) in
        mark := false;                      (* invalidate the mark *)
        r
      with e -> mark := false; raise e

end

(* ------------------------------------------------------------------------ *)
(* Simple heap *)
(* A mapping of keys to values. Priority is used for the sake of
   efficient operations. Also, values with the same priority are
   considered equivalent (belong to the same binding region)
   and are collapsed, lazily.

   The invariant: for each non-leaf
   node, the priority of the node is strictly greater than the priorities
   of any of the child nodes. The order of priorities between
   the children can be arbitrary.
*)

module Priority : sig

  type t

  val fresh : unit -> t

end = struct

  type t = int

  let counter = ref 0

  (* Keep in mind the invariant that variables of the same priority
     comes from the same binding location. So, we must keep the
     priorities unique to binders. Giving binders monotonically
     increasing priorities is helpful: the innermost binding
     has the highest priority and it will be at the top of the heap,
     the easiest to remove.
       *)
  let fresh () =
    incr counter;
    !counter

end

module Heap : sig

  type 'a t

  val empty : 'a t

  val singleton : Priority.t -> 'a -> 'a t

  val merge : 'a t -> 'a t -> 'a t

  val remove : 'a t -> Priority.t -> 'a t

  val choose : 'a t -> 'a option

  val iter : ('a -> unit) -> 'a t -> unit

end = struct

  type 'a t =
    | Nil
    | HNode of Priority.t * 'a * 'a t * 'a t

  let empty = Nil

  let singleton prio v =
    HNode (prio, v, Nil, Nil)

  let rec merge h1 h2 =
    match h1, h2 with
    | Nil, h | h, Nil-> h
    | HNode (p1, v1, l1, r1), HNode (p2, v2, l2, r2) -> begin
        match compare p1 p2 with
        | 0 -> HNode (p1, v1, merge l1 l2, merge r1 r2) (* same keys *)
        | n when n < 0 -> HNode (p2, v2, merge h1 l2, r2)
        | _ -> HNode (p1, v1, l1, merge h2 r1)
      end

  let rec remove h p =
    match h with
    | Nil -> Nil
    | HNode (pn, v, h1, h2) -> begin
        match compare p pn with
        | 0 -> merge h1 h2              (* p cannot occur in h1 or h2 *)
        | n when n > 0 -> h             (* entire tree has the lower priority *)
        | _ -> HNode (pn, v, remove h1 p, remove h2 p)
      end

  let choose = function
    | Nil -> None
    | HNode (_, v, _, _) -> Some v

  let rec iter f = function
    | Nil -> ()
    | HNode (_, v, h1, h2) -> begin
        f v;
        iter f h1;
        iter f h2
      end

end

(* ------------------------------------------------------------------------ *)
(* Simple map *)

module Symbol : sig

  type t

  val fresh : unit -> t

  val to_string : t -> string

  val compare : t -> t -> int

end = struct

  type t = int

  let counter = ref 0

  let fresh () =
    incr counter;
    !counter

  let to_string = string_of_int

  let compare (x : t) (y : t) = compare x y

end

module Map : sig

  type 'a t

  val empty : 'a t

  val singleton : Symbol.t -> 'a -> 'a t

  val add : Symbol.t -> 'a -> 'a t -> 'a t

  val find : Symbol.t -> 'a t -> 'a

  val find_list : Symbol.t -> 'a t list -> 'a

  val find_alist : Symbol.t -> ('a t * 'b) list -> 'a

  val iter : ('a -> unit) -> 'a t -> unit

  val merge : ('a -> 'a -> unit) -> 'a t -> 'a t -> 'a t

  val choose : 'a t -> 'a option

  val same : ('a -> unit) -> 'a t -> ('b -> unit) -> 'b t -> unit

end = struct

  module SymbolMap = Map.Make(Symbol)

  type 'a t = 'a SymbolMap.t

  let empty = SymbolMap.empty

  let singleton = SymbolMap.singleton

  let add = SymbolMap.add

  let find = SymbolMap.find

  let rec find_list sym = function
    | [] -> raise Not_found
    | heap :: rest ->
        match find sym heap with
        | exception Not_found -> find_list sym rest
        | v -> v

  let rec find_alist sym = function
    | [] -> raise Not_found
    | (heap, _) :: rest ->
        match find sym heap with
        | exception Not_found -> find_alist sym rest
        | v -> v

  let iter f t =
    SymbolMap.iter (fun _ v -> f v) t

  let merge intersection t1 t2 =
    SymbolMap.merge
      (fun x v1 v2 ->
        match v1, v2 with
        | None, None -> None
        | Some v, None -> Some v
        | None, Some v -> Some v
        | Some v1, Some v2  -> intersection v1 v2; None)
      t1 t2

  let choose t =
    match SymbolMap.choose t with
    | exception Not_found -> None
    | (_, v) -> Some v

  let same left t1 right t2 =
    ignore
      (SymbolMap.merge
         (fun x v1 v2 ->
           match v1, v2 with
           | None, None -> None
           | Some v, None -> left v; None
           | None, Some v -> right v; None
           | Some v, Some _  -> None)
         t1 t2)

end

(* ------------------------------------------------------------------------ *)
(* Representation of bound variables *)

module Var : sig

  type t

  val generate : Stackmark.t -> Priority.t -> string loc -> t

  val relocate : t -> Location.t -> t

  val name : t -> string loc

  val txt : t -> string

  val loc : t -> Location.t

  val heap : t -> t Heap.t

  val map : t -> t Map.t

end = struct

  type t = {
    name : string Location.loc;
    stackmark : Stackmark.t;
    symbol : Symbol.t;
    priority : Priority.t;
  }

  let generate stackmark priority name =
    let symbol = Symbol.fresh () in
    let txt = var.txt ^ "_" ^ (Symbol.to_string symbol) in
    let name = {name with txt} in
      { name; stackmark; symbol; priority }

  let relocate t loc =
    let name = { t.name with loc } in
      { t with name }

  let name t = t.name

  let txt t = t.name.txt

  let loc t = t.name.loc

  let heap t = Heap.singleton t t.priority

  let map t = Map.singleton t t.symbol

end

(* ------------------------------------------------------------------------ *)
(* Representation of expressions *)

module ExpRepr : sig

  type t =
    { heap : Var.t Heap.t;
      exp : Parsetree.expression; }

  (* Check an expression's free variables are in scope. *)
  val validate : Location.t -> t -> unit

  (* Combine the free variables of two expressions. *)
  val merge :
    Location.t -> Var.t Heap.t -> t -> Var.t Heap.t * Parsetree.expression

end  = struct

  type t =
    { heap : Var.t Heap.t;
      exp : Parsetree.expression; }

  (* This is a run-time error, rather than a translation-time error *)
  let scope_extrusion_error loc exp name =
    Format.fprintf Format.str_formatter
      "Scope extrusion detected at %a for expression built at %a \
       for the identifier %s bound at %a"
      Location.print loc Location.print exp.pexp_loc
      name.txt Location.print name.loc;
    failwith (Format.flush_str_formatter ())

  (* Check to make sure that free variables in the potentially open
     code fragment are valid.
     If it weren't for delimited control, the order of stack marks is
     stable; therefore, if the maximal mark is valid then all
     smaller marks are valid as well.
     Delimited control spoils all that.
     When we capture some of the inner-bindings
     in a continuation and then reinstall that continuation at the
     top level, the `latest' free variable is valid but earlier are
     no longer valid:

    let r = ref ... in
    <<fun x1 x2 -> $(reset <<fun y1 y2 ->
                                $(shift k (r := k; k <<0>>))>>)>>
    .r <<2>>
    Here, y1 and y2 are valid but x1 and x2 are not.
  *)
  let validate loc { heap; exp } =
    Heap.iter
      (fun var ->
        if not (var.stackmark ()) then
          scope_extrusion_error loc exp var.name)
      heap

  let merge _loc acc { heap; exp } =
    let heap = Heap.merge acc heap in
      (heap, exp)

end

(* ------------------------------------------------------------------------ *)
(* Representation of patterns *)

module PatRepr : sig

  type t =
    { map : Var.t Map.t;
      pat : Parsetree.pattern; }

  (* Check a pattern's binding variables are in scope. *)
  val validate : Location.t -> t -> unit

  (* Check a pattern binds no variables. *)
  val nonbinding : Location.t -> t -> unit

  (* Combine the binding variables of two patterns, checking that they
     are disjoint (used for sub-patterns). *)
  val merge :
    Location.t -> Var.t Map.t -> t -> Var.t Map.t * Parsetree.pattern

  (* Combine the binding variables of two patterns, checking that they
     are equivalent (used for or-patterns). *)
  val join :
    Location.t -> t -> t ->
    Var.t Map.t * Parsetree.pattern * Parsetree.pattern

end = struct

  type t =
    { map : Var.t Map.t;
      pat : Parsetree.pattern; }

  let scope_extrusion_error loc pat name =
    Format.fprintf Format.str_formatter
      "Scope extrusion detected at %a for pattern built at %a \
       for the identifier %s"
      Location.print loc Location.print pat.ppat_loc name.txt;
    failwith (Format.flush_str_formatter ())

  let validate loc { map; pat } =
    Map.iter
      (fun var ->
        if not (var.stackmark ()) then
          scope_extrusion_error loc pat var.name)
      map

  let duplicate_binding_error loc pat var =
    Format.fprintf Format.str_formatter
       "Duplicate bindings detected at %a for pattern built at %a \
        for the identifier %s already bound at %a"
      Location.print loc Location.print pat.ppat_loc
      var.txt Location.print var.loc;
    failwith (Format.flush_str_formatter ())

  let merge loc acc { map; pat } =
    let duplicate var _ = duplicate_binding_error loc pat var in
    let map = Map.merge duplicate acc map in
      (map, pat)

  let mismatch_binding_error loc pat1 pat2 var =
    Format.fprintf Format.str_formatter
       "Mismatched bindings detected at %a for patterns built at %a and %a \
        for the identifier %s"
      Location.print loc Location.print pat1.ppat_loc
      Location.print pat2.ppat_loc var.txt;
    failwith (Format.flush_str_formatter ())

  let join loc {map = map1; pat = pat1} {map = map2; pat = pat2} =
    let diff var = mismatch_binding_error loc pat1 pat2 var in
    Map.same diff map1 diff map2;
    map1, pat1, pat2

  let unbound_var_error loc pat var =
    Format.fprintf Format.str_formatter
      "Pattern for binding at %a built at %a does not bind the identifier %s"
      Location.print loc Location.print pat.ppat_loc
      Location.print occurred var.txt;
    failwith (Format.flush_str_formatter ())

  let additional_var_error loc pat var =
    Format.fprintf Format.str_formatter
      "Pattern for binding at %a built at %a binds additional identifier %s"
      Location.print loc Location.print pat.ppat_loc
      Location.print occurred var.txt;
    failwith (Format.flush_str_formatter ())

  let nonbinding loc { map; pat } =
    match Map.choose map with
    | None -> ()
    | Some var -> additional_var_error loc pat var

  let check_bindings loc vars { map; pat } =
    let full_map =
      List.fold_left
        (fun acc var -> Map.add var.symbol var acc)
        Map.empty vars
    in
    let unbound var = unbound_var_error loc pat var in
    let additional var = additional_var_error loc pat var in
      Map.same unbound full_map additional map

  let var t =
    { ppat_loc=s.loc; ppat_desc=Ppat_var t.name; ppat_attributes = [] }

end

(* ------------------------------------------------------------------------ *)
(* Representation of patterns *)

module CaseRepr : sig

  type t =
    { heap : Var.t Heap.t;
      case : Parsetree.case; }

  (* Combine the free variables of two cases. *)
  val merge :
    Location.t -> Var.t Heap.t -> t -> Var.t Heap.t * Parsetree.case

end = struct

  type t =
    { heap : Var.t Heap.t;
      case : Parsetree.case; }

   let merge _loc acc { heap; case } =
     let heap = Heap.merge acc heap in
       (heap, case)

end


(* ------------------------------------------------------------------------ *)

(* Bindings in the future stage *)
(* Recall, all bindings at the future stage are introduced by
   patterns, and hence are simple names, without any module qualifications.
*)

module Binding : sig

  val simple :
    Location.t -> string loc -> (t -> exp_repr) ->
    t Heap.t * Parsetree.pattern * Parsetree.expression

  val recursive :
    Location.t -> string loc list ->
    (t list -> expr_repr list * expr_repr) ->
    t Heap.t * (Parsetree.pattern * Parsetree.expression) list
    * Parsetree.expression

  val pattern :
    Location.t -> string loc list ->
    (t list -> pat_repr * exp_repr) ->
    t Heap.t * Parsetree.pattern * Parsetree.expression

  val guarded :
    Location.t -> string loc list ->
    (t list -> pat_repr * exp_repr option * exp_repr) ->
    t Heap.t * Parsetree.pattern *
    Parsetree.expression option * Parsetree.expression

end = struct

  (* Generate a fresh name off the given name, enter a new binding region
     and evaluate a function passing it the generated name as exp_repr.
     Remove the generated name from the annotation on the resulting code_exp.
     Return that result and the generated name.
     This function embodies the translation of simple functions, for-loops,
     simple let-expressions, etc.
  *)
  let simple loc name f =
    Stackmark.region
      (fun mark ->
        let prio = Priority.fresh () in
        let var = gensym mark prio name in
        let expr = f var in
        Exp.validate loc expr;
        let pat = Pat.var var in
        (Heap.remove prio heap, pat, expr.exp))

  let multiple loc names f =
    Stackmark.region
      (fun mark ->
        let prio = Priority.fresh () in
        let vars = List.map (gensym mark prio) names in
        let expr = f vars in
        Exp.validate loc heap expr;
        let pats = List.map Pat.var vars in
        (Heap.remove prio heap, pats, expr.exp))

  let pair_binding_error loc npats nexps =
    Format.fprintf Format.str_formatter
      "Binding at %a has %d patterns but %d expressions"
      Location.print loc npats nexps;
    failwith (Format.flush_str_formatter ())

  let pair_bindings loc pats exps =
    let npats = List.length pats in
    let nexps = List.length exps in
    if npats <> nexps then pair_binding_error loc npats nexps;
    List.combine pats exps

  let recursive loc names f =
    Stackmark.region
      (fun mark ->
        let prio = Priority.fresh () in
        let vars = List.map (gensym mark prio) names in
        let defs, expr = f vars in
        iter_list loc Exp.validate defs;
        Exp.validate loc expr;
        let heap, defs = accum_list loc Exp.merge expr.heap defs in
        let pats = List.map pat vars in
        let pairs = pair_bindings loc pats defs in
        (Heap.remove prio heap, pairs, expr.exp))

  let pattern loc names f =
    Stackmark.region
      (fun mark ->
        let prio = Priority.fresh () in
        let vars = List.map (gensym mark prio) names in
        let patr, expr = f vars in
        Pat.validate loc patr;
        Pat.check_bindings loc vars patr;
        Exp.validate loc expr;
        (Heap.remove prio expr.heap, patr.pat, expr.exp))

  let guarded loc names f =
    Stackmark.region
      (fun mark ->
        let prio = Priority.fresh () in
        let vars = List.map (gensym mark prio) names in
        let patr, guard, expr = f vars in
        Pat.validate loc map patr;
        Pat.check_bindings loc vars patr;
        iter_option loc Exp.validate guard;
        Exp.validate loc expr;
        let heap, guard = accum_option loc Exp.merge expr.heap guard in
        (Heap.remove prio heap, patr.pat, guard, expr.exp))

end

(* ------------------------------------------------------------------------ *)
(* Building Parsetree nodes *)

module Loc = struct

  type t = Location.t

  let none = Location.none

  let unmarshal (s : string) : t = Marshal.from_string s

end

module Constant = struct

  type t = constant

  let unmarshal str : t = Marshal.from_string str

end

(* Pattern builders *)
module Pat = struct

  type t = PatRepr.t

  open PatRepr

  let mk loc d =
    {ppat_desc = d; ppat_loc = loc; ppat_attributes = []}

  let any loc =
    (Map.empty, mk loc Ppat_any)

  let var loc var =
    let var = Var.relocate var loc in
    let name = Var.name var in
    let map = Var.map var in
      (map, mk loc (Ppat_var name))

  let alias loc pat var =
    validate loc pat;
    let var = Var.relocate var loc in
    let name = Var.name var in
    let map = Var.map var in
    let map, pat = merge loc map pat in
    let pat = mk loc (Ppat_alias(pat, name)) in
      (map, pat)

  let constant loc const =
    let map = Map.empty in
    let pat = mk loc (Ppat_constant const) in
      (map, pat)

  let interval loc const1 const2 =
    let map = Map.empty in
    let pat = mk loc (Ppat_interval(const1, const2)) in
      (map, pat)

  let tuple loc patl =
    iter_list loc validate patl;
    let map, patl = accum_list loc merge Map.empty patl in
    let pat = mk loc (Ppat_tuple patl) in
      (map, pat)

  let construct loc lid pato =
    iter_opt loc validate pato;
    let map, pato = accum_opt loc merge Map.empty pato in
    let pat = mk loc Ppat_construct(lid, pato) in
      (map, pat)

  let variant loc label pato =
    iter_opt loc validate pato;
    let map, pato = accum_opt loc merge Map.empty pato in
    let pat = mk loc (Ppat_variant(label, pato)) in
      (map, pat)

  let record loc patl closed =
    iter_alist loc validate patl;
    let map, patl = accum_alist loc merge Map.empty patl in
    let pat = mk loc (Ppat_record(patl, closed)) in
      (map, pat)

  let array loc patl =
    iter_list loc validate patl;
    let map, patl = accum_list loc merge Map.empty patl in
    let pat = mk loc (Ppat_array patl) in
      (map, pat)

  let or_ loc pat1 pat2 =
    validate loc pat1;
    validate loc pat2;
    let map, pat1, pat2 = join loc pat1 pat2 in
    let pat = mk loc (Ppat_or(pat1, pat2)) in
      (map, pat)

  let type_ loc lid =
    let map = Map.empty in
    let pat = mk loc (Ppat_type lid) in
      (map, pat)

  let lazy_ loc pat =
    validate loc pat;
    let map = pat.map in
    let pat = mk loc (Ppat_lazy pat.pat) in
      (map, pat)

  let exception_ loc pat =
    validate loc pat;
    let map = pat.map in
    let pat = mk loc (Ppat_exception pat.pat) in
      (map, pat)

end

module Case = struct

  type t = CaseRepr.t

  let nonbinding loc lhs guardo rhs =
    PatRepr.nonbinding loc lhs;
    iter_option loc ExpRepr.validate guardo;
    ExprRepr.validate loc rhs;
    let heap, guard = accum_option loc ExpRepr.merge rhs.heap expo in
    let case =
      { pc_lhs = lhs.pat; pc_guard = guard; pc_rhs = rhs.exp }
    in
      { heap; case }

  let binding loc names f =
    let heap, lhs, guard, rhs = Binding.guarded loc names f in
    let case =
      { pc_lhs = lhs; pc_guard = guard; pc_rhs = rhs }
    in
      { heap; case }

end

module Expr = struct

  (* The representation of the possibly open code: AST plus the
     set of free identifiers, annotated with the related marks
  *)
  type t = ExprRepr.t

  open ExpRepr

  (* Check that the code is closed and return the closed code *)
  module Closed = struct

    (* The closed code is AST *)
    type t = Parsetree.expression

    (* The same as close_code but return the closedness check as a thunk
       rather than performing it.
       This is useful for debugging and for showing the code
    *)
    let close_delay_check exp =
        match Heap.choose exp.heap with
        | None -> (exp.exp, fun () -> ())
        | Some var ->
          (exp.exp, fun () ->
            let name = Var.name var in
            Format.fprintf Format.str_formatter
            "The code built at %a is not closed: \
             identifier %s bound at %a is free"
            Location.print ast.pexp_loc name.txt Location.print name.loc;
            failwith (Format.flush_str_formatter ()))

    let close exp =
      let exp, check = close_delay_check exp in
      check (); exp

    let open_ exp =
      let heap = Heap.empty in
        { heap; exp }

  end

  let mk loc desc =
    { pexp_loc = loc; pexp_desc = desc; pexp_attributes = [] }

  let mk_vb loc p e =
    {pvb_pat = p;pvb_expr = e;pvb_loc = loc;pvb_attributes = [];}

  let var loc var =
    let heap = Var.heap var in
    let name = Var.name var in
    let lid = { name with txt = Longident.Lident name.txt } in
    let exp = mk loc (Pexp_ident lid) in
      { heap; exp }

  let ident loc lid =
    let heap = Heap.empty in
    let exp = mk loc (Pexp_ident lid) in
      { heap; exp }

  let constant loc const =
    let heap = Heap.empty in
    let exp = mk loc (Pexp_constant const) in
      { heap; exp }

  let let_simple loc name def f =
    validate loc def;
    let heap, pat, body = Binding.simple loc name f in
    let heap, def = merge heap def in
    let vb = mk_vb loc pat def in
    let exp = mk loc (Pexp_let(Nonrecursive, [vb], body)) in
      { heap; exp }

  let let_rec_simple loc name f =
    let heap, defs, body = Binding.recursive loc name f in
    let vbs = List.map (fun (pat, exp) -> Vb.mk loc pat exp) defs in
    let exp = mk loc (Pexp_let(Recursive, vbs, body)) in
      { heap; exp }

  let let_ loc names def f =
    validate loc def;
    let heap, pat, body = Binding.pattern loc names f in
    let heap, def = merge heap def in
    let vb = Vb.mk loc pat def in
    let exp = mk loc (Pexp_let(Nonrecursive, [vb], body)) in
      { heap; exp }

  let fun_nonbinding loc label pat exp =
    PatRepr.nonbinding loc pat;
    validate loc exp;
    let heap = exp.heap in
    let exp = mk loc (Pexp_fun (label, None, pat.pat, exp.exp)) in
    { heap; exp }

  let fun_simple loc name label default f =
    iter_opt loc validate default;
    let heap, pat, exp = Binding.simple loc name f in
    let heap, default = accum_opt loc merge heap default in
    let exp = mk loc (Pexp_fun (label, default, pat, exp)) in
      { heap; exp }

  let fun_ loc names label default f =
    iter_opt loc validate default;
    let heap, pat, exp = Binding.pattern loc names f in
    let heap, default = accum_opt loc merge heap default in
    let exp = mk loc (Pexp_fun(label, default, pat, exp)) in
      { heap; exp }

  let function_ loc cases =
    let heap, cases = accum_list loc CaseRepr.merge Heap.empty cases in
    let exp = mk loc (Pexp_function cases) in
      { heap; exp }

  let apply loc fn args =
    validate loc fn;
    iter_alist loc validate args;
    let heap, args = accum_alist loc merge fn.heap arg in
    let exp = mk loc (Pexp_apply (fn.exp, arg)) in
      { heap; exp }

  let match_ loc exp cases =
    validate loc exp;
    let heap, cases = accum_list loc CaseRepr.merge exp.heap cases in
    let exp = mk loc (Pexp_match(exp.exp, cases)) in
      { heap; exp }

  let try_ loc exp cases =
    validate loc exp;
    let heap, cases = accum_list loc CaseRepr.merge exp.heap cases in
    let exp = mk loc (Pexp_try(exp.exp, cases)) in
      { heap; exp }

  let tuple loc exps =
    iter_list loc validate exps;
    let heap, exps = accum_list loc merge Heap.empty exps in
    let exp = mk loc (Pexp_tuple exps) in
      { heap; exp }

  let construct loc lid argo =
    iter_opt loc validate argo;
    let heap, argo = accum_opt loc merge Heap.empty argo in
    let exp = mk loc (Pexp_construct (lid, argo)) in
      { heap; exp }

  let variant loc label argo =
    iter_opt loc validate argo;
    let heap, argo = accum_opt loc merge Heap.empty argo in
    let exp = mk loc (Pexp_variant (label, argo)) in
      { heap; exp }

  let record loc defs orig =
    iter_alist loc validate defs;
    iter_opt loc validate orig;
    let heap, defs = accum_alist loc merge Heap.empty defs in
    let heap, orig = accum_opt loc merge heap orig in
    let exp = mk loc (Pexp_record (defs, orig)) in
      { heap; exp }

  let field loc rcd lid =
    validate loc rcd;
    let heap = rcd.heap in
    let exp = mk loc (Pexp_field (rcd.exp, lid)) in
      { heap; exp }

  let setfield loc rcd lid def =
    validate loc rcd;
    validate loc def;
    let heap, def = merge loc rcd.heap def in
    let exp = mk loc (Pexp_setfield (rcd.exp, lid, def)) in
      { heap; exp }

  let array loc args =
    iter_list loc validate args;
    let heap, args = accum_list loc merge Heap.empty args in
    let exp = mk loc (Pexp_array args) in
      { heap; exp }

  let ifthenelse loc cond tr fs =
    validate loc cond;
    validate loc tr;
    iter_opt loc validate fs;
    let heap, tr = merge loc cond.heap tr in
    let heap, fs = accum_opt loc merge heap fs in
    let exp = mk loc (Pexp_ifthenelse (cond.exp, tr, fs)) in
      { heap; exp }

  let sequence loc exp1 exp2 =
    validate loc exp1;
    validate loc exp2;
    let heap, exp2 = merge loc exp1.heap exp2 in
    let exp = mk loc (Pexp_sequence(exp1.exp, exp2)) in
      { heap; exp }

  let for_nonbinding loc pat low high dir body =
    PatRepr.nonbinding loc pat;
    validate loc low;
    validate loc high;
    validate loc body;
    let heap, high = merge loc low.heap high in
    let heap, body = merge loc heap body in
    let exp = mk loc (Pexp_for (pat, low.exp, high, dir, body)) in
      { heap; exp }

  let for_simple loc name low high dir f =
    validate loc low;
    validate loc high;
    let heap, pat, body = Binding.simple loc name f in
    let heap, low = merge loc heap low in
    let heap, high = merge loc heap, high in
    let exp = mk loc (Pexp_for (pat, low, high, dir, body)) in
      { heap; exp }

  let send loc obj meth =
    validate loc obj;
    let heap = obj.heap in
    let exp = mk loc (Pexp_send(obj, meth)) in
      { heap; exp }

  let assert_ loc exp =
    validate loc exp;
    let heap = exp.heap in
    let exp = mk loc (Pexp_assert exp) in
      { heap; exp }

  let lazy_ loc exp =
    validate loc exp;
    let heap = exp.heap in
    let exp = mk loc (Pexp_lazy exp) in
      { heap; exp }

  let open_ loc ovr lid exp =
    validate loc exp;
    let heap = exp.heap in
    let exp = mk loc (Pexp_open(ovr, lid, exp)) in
      { heap; exp }

  let quote loc a =
    validate loc exp;
    let heap = exp.heap in
    let exp = mk loc (Pexp_quote exp) in
      { heap; exp }

  let escape loc a =
    validate loc exp;
    let heap = exp.heap in
    let exp = mk loc (Pexp_escape exp) in
      { heap; exp }

end
