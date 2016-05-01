
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
  type stackmark = unit -> bool           (* true if valid *)

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

  val singleton : 'a t -> Priority.t -> 'a -> 'a t

  val merge : 'a t -> 'a t -> 'a t

  val merge_alist : ('a t * 'b) list -> 'a t * 'b list

  val remove : 'a t -> Priority.t -> 'a t

  val choose : 'a t -> 'a option

  val iter : ('a -> unit) -> 'a t -> unit

end = struct

  type 'a t =
    | Nil
    | HNode of Priority.t * 'a * 'a heap * 'a heap

  let empty = Nil

  let singleton h prio v =
    HNode (p, v, Nil, Nil)

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
        | _ -> HNode (pn, v, remove p h1, remove p h2)
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

  let to_string = int_to_string

  let compare (x : t) (y : t) = compare x y

end

module Map : sig

  type 'a t

  val empty : 'a t

  val singleton : 'a t -> Symbol.t -> 'a -> 'a t

  val add : Symbol.t -> 'a -> 'a t -> 'a t

  val find : Symbol.t -> 'a t -> 'a

  val find_list : Symbol.t -> 'a t list -> 'a

  val find_alist : Symbol.t -> ('a t * 'b) list -> 'a

  val iter : ('a -> unit) -> 'a t -> unit

  val merge : ('a -> 'a -> unit) -> 'a t -> 'a t -> 'a t

  val merge_alist : ('a t * 'b) list -> 'a t * 'b list

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
        | Some v1, Some v2  -> intersection v1 v2; None
      t1 t2

  let merge_alist xl =
    map_accum
      (fun acc (h, x) ->
         let acc = merge h acc in
           acc, x)
      SymbolMap.empty xl

  let choose = SymbolMap.choose

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

(* Bindings in the future stage *)
(* Recall, all bindings at the future stage are introduced by
   patterns, and hence are simple names, without any module qualifications.
*)
module Var : sig

  type t

  val txt : t -> string

  val loc : t -> Location.t

  module Exp : sig

    val use : t -> t Heap.t * string loc

    (* Check an expression's free variables are in scope. *)
    val validate :
      Location.t -> (t Heap.t * Parsetree.expression) -> unit

    (* Combine the free variables of two expressions. *)
    val merge :
      Location.t -> t Heap.t -> (t Heap.t * Parsetree.expression) ->
      t Heap.t * Parsetree.expression

  end

  module Pat : sig

    val use : t -> t Map.t * string loc

    (* Check a pattern's binding variables are in scope. *)
    val validate :
      Location.t -> (t Map.t * Parsetree.pattern) -> unit

    (* Check a pattern binds no variables. *)
    val nonbinding :
      Location.t -> t Map.t -> Parsetree.pattern -> unit

    (* Combine the binding variables of two patterns, checking that they
       are disjoint (used for sub-patterns). *)
    val merge :
      Location.t -> t Map.t -> (t Map.t * Parsetree.pattern) ->
      t Map.t * Parsetree.pattern

    (* Combine the binding variables of two patterns, checking that they
       are equivalent (used for or-patterns). *)
    val join :
      Location.t -> (t Map.t * Parsetree.pattern) ->
      (t Map.t * Parsetree.pattern) -> t Map.t * Parsetree.pattern

  end

  val with_simple_binding :
    Location.t -> string loc -> (t -> t Heap.t * Parsetree.expression) ->
    Parsetree.pattern * t Heap.t * Parsetree.expression

  val with_simple_bindings :
    Location.t -> string loc list ->
    (t list -> t Heap.t * Parsetree.expression) ->
    Parsetree.pattern list * t Heap.t * Parsetree.expression

  val with_simple_rec_bindings :
    Location.t -> string loc list ->
    (t list -> (t Heap.t * Parsetree.expression) list
               * (t Heap.t * Parsetree.expression)) ->
    t Heap.t * (Parsetree.pattern * Parsetree.expression) list
    * Parsetree.expression

  val with_bindings :
    Location.t -> string loc list ->
    (t list -> (t Map.t * Parsetree.pattern)
               * (t Heap.t * Parsetree.expression)) ->
    Parsetree.pattern * t Heap.t * Parsetree.expression

end = struct

  type t = {
    name : string loc;
    stackmark : Stackmark.t;
    symbol : Symbol.t;
    priority : Priority.t;
  }

  let txt { name } = name.txt

  let loc { name } = name.loc

  module Exp = struct

    let use t = (Heap.singleton t t.priority, t.name)

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
    let validate loc (heap, exp) =
      Heap.iter
        (fun var ->
          if not (var.stackmark ()) then
            scope_extrusion_error loc exp var.name)
        heap

    let merge _loc heap1 (heap2, exp2) =
      Heap.merge heap1 heap2, exp2

  end

  module Pat = struct

    let use t = (Map.singleton t t.symbol, t.name)

    (* This is a run-time error, rather than a translation-time error *)
    let scope_extrusion_error loc pat name =
      Format.fprintf Format.str_formatter
        "Scope extrusion detected at %a for pattern built at %a \
         for the identifier %s"
        Location.print loc Location.print pat.ppat_loc name.txt;
      failwith (Format.flush_str_formatter ())

    let validate loc (map, pat) =
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

    let merge loc acc (map, pat) =
      let duplicate var _ = duplicate_binding_error loc pat var in
        Map.merge duplicate acc map, pat

    let mismatch_binding_error loc pat1 pat2 var =
      Format.fprintf Format.str_formatter
         "Mismatched bindings detected at %a for patterns built at %a and %a \
          for the identifier %s"
        Location.print loc Location.print pat1.ppat_loc
        Location.print pat2.ppat_loc var.txt;
      failwith (Format.flush_str_formatter ())

    let join loc (map1, pat1) (map2, pat2) =
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

    let nonbinding loc map pat =
      match Map.choose map with
      | None -> ()
      | Some var -> additional_var_error loc pat var

    let check_bindings loc vars map pat =
      let full_map =
        List.fold_left
          (fun acc var -> Map.add var.symbol var acc)
          Map.empty vars
      in
      let unbound var = unbound_var_error loc pat var in
      let additional var = additional_var_error loc pat var in
        Map.same unbound full_map additional map

    let var t =
      let (map, s) = use t in
      let pat =
        { ppat_loc=s.loc; ppat_desc=Ppat_var s; ppat_attributes = [] }
      in
        (map, pat)

  end

  let gensym stackmark priority name =
    let symbol = Symbol.fresh () in
    let txt = var.txt ^ "_" ^ (Symbol.to_string symbol) in
    let name = {name with txt} in
      { name; stackmark; symbol; priority }

  (* Generate a fresh name off the given name, enter a new binding region
     and evaluate a function passing it the generated name as exp_repr.
     Remove the generated name from the annotation on the resulting code_exp.
     Return that result and the generated name.
     This function embodies the translation of simple functions, for-loops,
     simple let-expressions, etc.
  *)
  let with_simple_binding loc name f =
    Stackmark.region
      (fun mark ->
        let prio = Priority.fresh () in
        let var = gensym mark prio name in
        let (heap, exp) = f var in
        Exp.validate loc heap exp;
        let pat = Pat.var var in
        (pat, Heap.remove prio heap, exp))

  let with_simple_bindings loc names f =
    Stackmark.region
      (fun mark ->
        let prio = Priority.fresh () in
        let vars = List.map (gensym mark prio) names in
        let (heap, exp) = f vars in
        Exp.validate loc heap exp;
        let pats = List.map Pat.var vars in
        (pats, Heap.remove prio heap, exp))

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

  let with_simple_rec_bindings loc names f =
    Stackmark.region
      (fun mark ->
        let prio = Priority.fresh () in
        let vars = List.map (gensym mark prio) names in
        let hexps, (bheap, bexp) = f vars in
        List.iter (fun (heap, exp) -> Exp.validate loc heap exp) hexps;
        Exp.validate loc bheap bexp;
        let heap, exps = Heap.merge_alist hexps in
        let heap = Heap.merge heap bheap in
        let pats = List.map pat vars in
        let pairs = pair_bindings loc pats exps in
        (Heap.remove prio heap, pairs, bexp))

  let with_bindings loc names f =
    Stackmark.region
      (fun mark ->
        let prio = Priority.fresh () in
        let vars = List.map (gensym mark prio) names in
        let ((map, pat), (heap, exp)) = f vars in
        Exp.validate loc heap exp;
        Pat.validate loc map pat;
        Pat.check_bindings loc vars map pat;
        (pat, Heap.remove prio heap, exp))

end

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
  | (_, x) :: rest -> f loc x; iter_list loc f rest

(* ------------------------------------------------------------------------ *)
(* Accumulating maps over common structures *)

let accum_opt loc f acc o =
  match o with
  | None -> (acc, None)
  | Some x ->
      let (acc, x) = f loc acc x in
        (acc, Some x)

let rec accum_list loc f acc l =
  match l with
  | [] -> (acc, [])
  | x :: rest ->
      let (acc, x) = f loc acc x in
      let (acc, rest) = accum_list loc f acc rest in
        (acc, x :: rest)

let rec accum_alist loc f acc l =
  match l with
  | [] -> (acc, [])
  | (k, x) :: rest ->
      let (acc, x) = f loc acc x in
      let (acc, rest) = accum_alist f acc rest in
        (acc, (k, x) :: rest)


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

  let mk loc d =
    {ppat_desc = d; ppat_loc = loc; ppat_attributes = []}

  let any loc =
    (Map.empty, mk loc Ppat_any)

  let var loc var =
    let var = Var.Pat.use var in
      (fst var, mk loc (Ppat_var (snd var)))

  let alias loc pat var =
    Var.Pat.validate loc pat;
    let var = Var.Pat.use var in
    let map = Var.Pat.merge loc (fst var) pat in
    let pat = mk loc (Ppat_alias(snd pat, snd var)) in
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
    iter_list loc Var.Pat.validate patl;
    let map, patl = accum_list loc Var.Pat.merge Map.empty patl in
    let pat = mk loc (Ppat_tuple patl) in
      (map, pat)

  let construct loc lid pato =
    iter_opt loc Var.Pat.validate pato;
    let map, pato = accum_opt loc Var.Pat.merge_option Map.empty pato in
    let pat = mk loc Ppat_construct(lid, pato) in
      (map, pat)

  let variant loc label pato =
    iter_opt loc Var.Pat.validate_option pato;
    let map, pato = accum_opt loc Var.Pat.merge_option Map.empty pato in
    let pat = mk loc (Ppat_variant(label, pato)) in
      (map, pat)

  let record loc patl closed =
    iter_alist loc Var.Pat.validate patl;
    let map, patl = accum_alist loc Var.Pat.merge_alist Map.empty patl in
    let pat = mk loc (Ppat_record(patl, closed)) in
      (map, pat)

  let array loc patl =
    iter_list loc Var.Pat.validate patl;
    let map, patl = accum_list loc Var.Pat.merge_list Map.empty patl in
    let pat = mk loc (Ppat_array patl) in
      (map, pat)

  let or_ loc pat1 pat2 =
    Var.Pat.validate loc pat1;
    Var.Pat.validate loc pat2;
    let map, pat1, pat2 = Var.Pat.join loc pat1 pat2 in
    let pat = mk loc (Ppat_or(pat1, pat2)) in
      (map, pat)

  let type_ loc lid =
    let map = Map.empty in
    let pat = mk loc (Ppat_type lid) in
      (map, pat)

  let lazy_ loc (map, pat) =
    Var.Pat.validate loc map pat;
    let pat = mk loc (Ppat_lazy pat) in
      (map, pat)

  let exception_ loc (map, pat) =
    Var.Pat.validate loc map pat;
    let pat = mk loc (Ppat_exception pat) in
      (map, pat)

end

module Vb = struct
  let mk loc p e =
    {pvb_pat = p;pvb_expr = e;pvb_loc = loc;pvb_attributes = [];}
end

module Expr = struct

  (* The representation of the possibly open code: AST plus the
     set of free identifiers, annotated with the related marks
  *)
  type t = Var.t Heap.t * Parsetree.expression

  (* Check that the code is closed and return the closed code *)
  module Closed = struct

    (* The closed code is AST *)
    type t = Parsetree.expression

    (* The same as close_code but return the closedness check as a thunk
       rather than performing it.
       This is useful for debugging and for showing the code
    *)
    let close_delay_check (heap, exp) =
        match Heap.choose heap with
        | None -> (exp, fun () -> ())
        | Some var ->
          (exp, fun () ->
            let txt = Var.txt var in
            let loc = Var.loc var in
            Format.fprintf Format.str_formatter
            "The code built at %a is not closed: \
             identifier %s bound at %a is free"
            Location.print ast.pexp_loc txt Location.print loc;
            failwith (Format.flush_str_formatter ()))

    let close expr =
      let (t, check) = close_delay_check expr in
      check (); ast

    let open_ t =
      (Heap.empty, t)

  end

  let mk loc desc =
    { pexp_loc = loc; pexp_desc = desc; pexp_attributes = [] }

  let var loc var =
    let (heap, var) = Var.Exp.use var in
    let lid = { var with txt = Longident.Lident var.txt } in
    let exp = mk loc (Pexp_ident lid) in
      (heap, exp)

  let ident loc lid =
    let heap = Heap.empty in
    let exp = mk loc (Pexp_ident lid) in
      (heap, exp)

  let constant loc const =
    let heap = Heap.empty in
    let exp = mk loc (Pexp_constant const) in
      (heap, exp)

  let let_simple loc name (heap1, exp1) f2 =
    Var.Exp.validate loc heap1 exp1;
    let (pat, heap2, exp2) = Var.with_simple_binding loc name f2 in
    let heap = Var.Exp.merge heap1 exp1 heap2 exp2 in
    let vb1 = Vb.mk loc pat exp1 in
    let exp = mk loc (Pexp_let(Nonrecursive, [vb1], exp2)) in
      (heap, exp)

  let let_rec_simple loc name f =
    Var.Exp.validate loc heap1 exp1;
    let (heap, pat_exp, body) = Var.with_simple_rec_bindings loc name f2 in
    let vbs = List.map (fun (pat, exp) -> Vb.mk loc pat exp) in
    let exp = mk loc (Pexp_let(Recursive, vbs, body)) in
      (heap, exp)

  let let_ loc names (heap1, exp1) f2 =
    Var.Exp.validate loc heap1 exp1;
    let (pat, heap2, exp2) = Var.with_bindings loc names f2 in
    let heap = Var.Exp.merge heap1 exp1 heap2 exp2 in
    let vb1 = Vb.mk loc pat exp1 in
    let exp = mk loc (Pexp_let(Nonrecursive, [vb1], exp2)) in
      (heap, exp)

  let fun_nonbinding loc label (map, pat) (heap, exp) =
    Var.Pat.nonbinding loc map1 pat;
    Var.Exp.validate loc heap exp;
    let exp = mk loc (Pexp_fun (label, None, pat, exp) in
    (heap, exp)

  let fun_simple loc name label f =
    let (pat, heap, exp) = Var.with_simple_binding loc name f in
    (heap, mk loc (Pexp_fun (label, None, pat, exp))

  let fun_ loc names label expo1 f2 =
    Var.Exp.validate_option loc expo1;
    let (heap1, expo1) = Var.Exp.merge_option loc expo1 in
    let (pat, heap2, exp2) = Var.with_bindings loc names f2 in
    let heap = Heap.merge heap1 heap2 in
    (heap, (Pexp_fun(label, expo1, pat, exp2)))

  let function_nonbinding loc cases =
    let cases =
      List.map
        (fun ((map, pat), expo, (heap, exp)) ->
          Var.Pat.nonbinding loc map pat;
          Var.Exp.validate_option loc expo;
          Var.Exp.validate loc heap exp;
          
                 
        )
      cases
    in
    let (vars, cases) =
      map_merge
        (fun loc (p, (eo, e)) ->
           let (vars1, eo) = validate_vars_option loc eo in
           let (vars2, e)  = validate_vars loc e in
             (merge vars1 vars2, {pc_lhs=p;pc_guard=eo;pc_rhs=e})
        loc a
    in
    (vars, mk loc (Pexp_function a))

  let function_ loc label pon fgbodies =
    prepare_cases loc Nil pon fgbodies
    @@ fun cases -> mk loc (Pexp_function cases)

  let apply loc a b =
    let (vars1, a) = validate_vars loc a in
    let (vars2, b) = validate_vars_list loc b in
      (merge vars1 vars2, mk loc (Pexp_apply (a, b)))

  let match_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_match (a, b))
  let try_ ?loc ?attrs a b = mk ?loc ?attrs (Pexp_try (a, b))

  let tuple loc a =
    let (vars, a) = validate_vars_list loc a in
    (vars, mk loc (Pexp_tuple a))

  let construct loc a b =
    let (vars, b) = validate_vars_option loc b in
    (vars, mk loc (Pexp_construct (a, b)))

  let variant loc a b =
    let (vars, b) = validate_vars_option loc b in
    (vars, mk loc (Pexp_variant (a, b)))

  let record loc a b =
    let (vars1, a) = validate_vars_alist loc a in
    let (vars2, b) = validate_vars_option loc eo in
    (merge vars1 vars2, mk loc (Pexp_record (a, b)))

  let field loc a b =
    let (vars, a) = validate_vars loc a in
    (vars, (Pexp_field (a, b)))

  let setfield loc a b c =
    let (vars1, a) = validate_vars loc a in
    let (vars2, c) = validate_vars loc c in
    Code (merge vars1 vars2, (Pexp_setfield (a, b, c)))

  let array loc a =
    let (vars, a) = validate_vars_list loc a in
    (vars, mk loc (Pexp_array a))

  let ifthenelse loc a b c =
    let (vars1, a) = validate_vars loc a in
    let (vars2, b) = validate_vars loc b in
    let (vars3, c) = validate_vars_option loc c in
    (merge vars1 (merge vars2 vars3), mk loc (Pexp_ifthenelse (a, b, c))

  let sequence loc a b =
    let (vars1, a) = validate_vars loc a in
    let (vars2, b) = validate_vars loc b in
    (merge vars1 vars2, mk loc Pexp_sequence(a, b))

  let for_nonbinding loc a b c d e =
    let (vars1, b) = validate_vars loc b in
    let (vars2, c) = validate_vars loc c in
    let (vars3, e) = validate_vars loc e in
    (merge vars1 (merge vars2 vars3),
     mk loc (Pexp_for (a, b, c, d, e))

  let for_simple loc name elo ehi dir fbody =
    let (vars1, elo) = validate_vars loc elo in
    let (vars2, ehi) = validate_vars loc ehi in
    let (name, vars3, ebody) = with_binding_region loc old_name fbody in
    let pat = Pat.var name.loc name in
    (merge vars1 (merge vars2 vars3),
     mk loc (Pexp_for (pat, elo, ehi, dir, ebody))

  let send loc a b =
    let (vars, a) = validate_vars loc a in
    (vars, (Pexp_send (a, b)))

  let assert_ loc a =
    let (vars, a) = validate_vars l a in
    (vars, mk loc (Pexp_assert a))

  let lazy_ loc a =
    let (vars, a) = validate_vars l a in
    (vars, mk loc (Pexp_lazy a))

  let open_ loc a b c = mk ?loc ?attrs
    let (vars, c) = validate_vars loc c in
    (vars, mk loc (Pexp_open (a, b, c)))

  let quote loc a =
    let (vars, a) = validate_vars l a in
    (vars, mk loc (Pexp_quote a))

  let escape loc a =
    let (vars, a) = validate_vars l a in
    (vars, mk loc (Pexp_escape a))

end
