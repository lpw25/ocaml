
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

module Heap : sig

  type prio

  val fresh : unit -> prio

  type 'a t

  val empty : 'a t

  val singleton : 'a t -> prio -> 'a -> 'a t

  val remove : 'a t -> prio -> 'a t

  val merge : 'a t -> 'a t -> 'a t

end = struct

  type prio = int

  let prio_counter = ref 0

  let fresh () =
    incr prio_counter;
    !prio_counter

  type 'a t =
    | Nil
    | HNode of prio * 'a * 'a heap * 'a heap

  let empty = Nil

  let singleton h prio v =
    HNode (p, v, Nil, Nil)

  let rec remove h p =
    match h with
    | Nil -> Nil
    | HNode (pn, v, h1, h2) -> begin
        match p - pn with
        | 0 -> merge h1 h2              (* p cannot occur in h1 or h2 *)
        | n when n > 0 -> h             (* entire tree has the lower prio *)
        | _ -> HNode (pn, v, remove p h1, remove p h2)
      end

  let rec merge h1 h2 =
    match h1, h2 with
    | Nil, h | h, Nil-> h
    | HNode (p1, v1, l1, r1), HNode (p2, v2, l2, r2) -> begin
        match p1 - p2 with
        | 0 -> HNode (p1, v1, merge l1 l2, merge r1 r2) (* same keys *)
        | n when n < 0 -> HNode (p2, v2, merge h1 l2, r2)
        | _ -> HNode (p1, v1, l1, merge h2 r1)
      end

  let choose = function
    | Nil -> None
    | HNode (_, v, _, _) -> Some v

  let rec for_all f = function
    | Nil -> true
    | HNode (_, v, h1, h2) ->
        f v && for_all f h1 && for_all f check h2

end


(* ------------------------------------------------------------------------ *)

(* Bindings in the future stage *)
(* Recall, all bindings at the future stage are introduced by
   patterns, and hence are simple names, without any module qualifications.
*)
type var_repr = {
  name : string loc;
  sym : int;
  priority : prio;
}

let gensym_count = ref 0

(* Make a simple identifier unique *)
let gensym (var : string loc) : var_repr =
  incr gensym_count;
  let sym = !gensym_count in
  let name = name.txt ^ "_" ^ sym in
  {var with txt = name}, sym

(* The representation of the possibly open code: AST plus the
   set of free identifiers, annotated with the marks
   of the corresponding with_binding_region forms
*)
type expr_repr = string loc heap * Parsetree.expression

(* The closed code is AST *)
type closed_expr_repr = Parsetree.expression

(* Check that the code is closed and return the closed code *)

(* The same as close_code but return the closedness check as a thunk
   rather than performing it.
   This is useful for debugging and for showing the code
*)
let close_code_delay_check : expr_repr -> closed_expr_repr * (unit -> unit) =
 function
  | (Nil,ast) -> (ast,fun () -> ())
  | (HNode (_,_,var,_,_),ast) ->
    (ast, fun () ->
      Format.fprintf Format.str_formatter
      "The code built at %a is not closed: identifier %s bound at %a is free"
      Location.print ast.pexp_loc var.txt Location.print var.loc;
      failwith (Format.flush_str_formatter ()))

let close_expr_repr : expr_repr -> closed_expr_repr = fun cde ->
  let (ast, check) = close_code_delay_check cde in
  check (); ast

let open_code : closed_expr_repr -> expr_repr = fun ast ->
  (Nil, ast)

(* left-to-right accumulating map *)
let rec map_accum : ('accum -> 'a -> 'accum * 'b) -> 'accum -> 'a list ->
  'accum * 'b list =
  fun f acc l ->
    match l with
    | []   -> (acc, [])
    | hd :: tl ->
        let (acc, hd) = f acc hd in
        let (acc, tl) = map_accum f acc tl in
          (acc, hd :: tl)

let map_merge : Location.t -> (Location.t -> 'a -> string loc heap * 'b) ->
                'a list -> string loc heap * 'b list =
  fun f loc xl ->
    map_accum
      (fun acc x ->
         let vars, y = f loc x in
         let acc = merge vars acc in
           acc, y)
      Nil xl

(* This is a run-time error, rather than a translation-time error *)
let scope_extrusion_error :
  detected:Location.t -> occurred:Location.t -> string loc -> 'a =
  fun ~detected ~occurred var ->
  Format.fprintf Format.str_formatter
    "Scope extrusion detected at %a for code built at %a for the identifier %s bound at %a"
    Location.print detected Location.print occurred
    var.txt Location.print var.loc;
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
let validate_vars : Location.t -> expr_repr -> expr_repr =
  fun l -> function
  | (Nil,_) as cde -> cde
  | (h, ast) as cde -> begin
      let rec check = function
        | Nil -> ()
        | HNode (_,sm,var,h1,h2) ->
            if sm () then (check h1; check h2)
            else scope_extrusion_error ~detected:l ~occurred:ast.pexp_loc var
      in check h; cde
  end

let validate_vars_option : Location.t -> expr_repr option ->
  string loc heap * Parsetree.expression option =
  fun loc co ->
    match co with
    | None -> (Nil, None)
    | Some c ->
        let (vars, c) = validate_vars loc c in
          (vars, Some c)

let validate_vars_list : Location.t -> expr_repr list ->
  string loc heap * Parsetree.expression list =
  fun loc cs ->
    map_merge validate_vars loc cs

let validate_vars_alist : Location.t -> ('a * expr_repr) list ->
  string loc heap * ('a * Parsetree.expression) list =
  fun loc cs ->
    map_merge
      (fun loc (a, c) ->
         let (vars, c) = validate_vars loc c in
           (vars, (a, c)))
      loc cs

(* Generate a fresh name off the given name, enter a new binding region
   and evaluate a function passing it the generated name as expr_repr.
   Remove the generated name from the annotation on the resulting code_expr.
   Return that result and the generated name.
   This function embodies the translation of simple functions, for-loops,
   simple let-expressions, etc.
*)
      (* Counter for assigning priorities to vars heap nodes. *)
      (* Keep in mind the invariant that variables of the same priority
         comes from the same binding location. So, we must keep the
         priorities unique to binders. Giving binders monotonically
         increasing priorities is helpful: the innermost binding
         has the highest priority and it will be at the top of the heap,
         the easiest to remove.
       *)
let with_binding_region :
  Location.t -> string loc -> (expr_repr -> expr_repr) ->
  string loc * string loc heap * Parsetree.expression = fun l name f ->
  let new_name = genident name in
  let (vars,e) =
   !with_stack_mark.stackmark_region_fn (fun mark ->
     incr prio_counter;
     let prio = !prio_counter in
     let var_code = (* code that corresponds to the bound variable *)
       (HNode (prio,mark,new_name,Nil,Nil),
          Ast_helper.Exp.mk ~loc:name.loc   (* the loc of the binder *)
           (Pexp_ident (mkloc (Longident.Lident new_name.txt) new_name.loc))) in
     let (vars,e) = validate_vars l (f var_code) in
     (remove prio vars, e)) in
  (new_name, vars, e)

let with_binding_regions :
  Location.t -> string loc -> (expr_repr -> expr_repr) ->
  string loc * string loc heap * Parsetree.expression = fun l names f ->
  let new_names = List.map genident names in
  let (vars,e) =
   !with_stack_mark.stackmark_region_fn (fun mark ->
     incr prio_counter;
     let prio = !prio_counter in
     let vars_code = (* code that corresponds to the bound variable *)
       List.map
         (fun new_name ->
          (HNode (prio,mark,new_name,Nil,Nil),
           Ast_helper.Exp.mk ~loc:name.loc   (* the loc of the binder *)
             (Pexp_ident (mkloc (Longident.Lident new_name.txt) new_name.loc))))
         new_names
     in
     let (vars,e) = validate_vars l (f vars_code) in
     (remove prio vars, e)) in
  (new_name, vars, e)

(* The most general version with several bindings and several expressions
   that use the bindings
 *)
let with_binding_region_gen :
  Location.t -> string loc list ->
  (Location.t -> 'a -> 'b * string loc heap) -> (expr_repr list -> 'a list) ->
  string loc list * string loc heap * 'b list
  = fun l names tr f ->
  let new_names = List.map genident names in
  let (vars,es) =
   !with_stack_mark.stackmark_region_fn (fun mark ->
     incr prio_counter;
     let prio = !prio_counter in
     let vars_code = (List.map (fun new_name ->
                      (* code that corresponds to a bound variable *)
       (HNode (prio,mark,new_name,Nil,Nil),
          Ast_helper.Exp.mk ~loc:new_name.loc    (* the loc of the binder *)
            (Pexp_ident (mkloc (Longident.Lident new_name.txt) new_name.loc))))
       new_names) in
     let cs = f vars_code in
     let (vars,es) = merge_map l tr cs in
       (remove prio vars, es)) in
  (new_names, vars, es)

(* ------------------------------------------------------------------------ *)
(* Building Parsetree nodes *)

(* Templates for building Parsetree/Typedtree components *)

(* Local reference: trx.cmi is available but location.cmi is not
   necessarily is in the current path.
*)
let loc_none = Location.none

let dummy_lid : string -> Longident.t loc = fun name ->
  Location.mknoloc (Longident.Lident name)

(* Exported. Used as a template for constructing lid expressions *)
let sample_lid = dummy_lid "*sample*"

(* Exported. Used as a template for constructing name expression *)
let sample_name : string loc = mknoloc "*sample*"

(* Exported. Used as a template for constructing pattern lists expressions *)
let sample_pat_list : Parsetree.pattern list = []
let sample_pats_names : Parsetree.pattern list * string loc list = ([],[])

(* Location builders *)
module Loc = struct

  let unmarshal str : location = Marshal.from_string str

end

module Constant = struct

  let unmarshal str : constant = Marshal.from_string str

end

(* Pattern builders *)
module Pat = struct

  let mk loc d =
    {ppat_desc = d; ppat_loc = loc; ppat_attributes = []}

  let any loc =
    mk loc Ppat_any


  | Ppat_alias of pattern * string loc
  | Ppat_constant of constant
  | Ppat_interval of constant * constant
  | Ppat_tuple of pattern list
  | Ppat_construct of Longident.t loc * pattern option
  | Ppat_variant of label * pattern option
  | Ppat_record of (Longident.t loc * pattern) list * closed_flag
  | Ppat_array of pattern list
  | Ppat_or of pattern * pattern
  | Ppat_constraint of pattern * core_type
  | Ppat_type of Longident.t loc
  | Ppat_lazy of pattern
  | Ppat_unpack of string loc
  | Ppat_exception of pattern
  | Ppat_extension of extension

end

(* Substitute the names of bound variables in the pattern.
   The new names are given in the string loc list. We
   take advantage of the fact that patterns are linear and
   the list of new names is ordered, in the order the bound
   variables occur in the pattern. Therefore, we substitute based
   on position.
   OR-patterns bring complexity however: both branches of an OR
   pattern bind exactly the same variables (but the order of
   variable occurrence within branches may be different).
   So for OR patterns we substitute by name, taking advantage
   of the fact the new names differ from the old ones in _nnn
   suffix. OR patterns are uncommon, so the complication of their processing
   is not that bad.

   This function is closely related to trx_pattern; It relies on the
   same pattern traversal order as trx_pattern.
 *)

(* two strings are the same up to (and including) n *)
let rec same_upto s1 s2 n =
  n < 0 || (s1.[n] = s2.[n] && same_upto s1 s2 (n-1))

let rec pattern_subst : ?by_name:bool ->
    string loc list -> Parsetree.pattern ->
     Parsetree.pattern * string loc list = fun ?(by_name=false) acc pat ->
 if acc = [] then (pat,acc) else           (* no more variables to subst *)
 let subst old_name acc =
   if by_name then begin
     let new_name =
       try List.find (fun n ->
         same_upto old_name.txt n.txt (String.rindex n.txt '_' - 1)) acc
       with _ ->
         begin
           Format.fprintf Format.str_formatter "old_name %s %a\n"
             old_name.txt Location.print old_name.loc;
           List.iter (fun n -> Format.fprintf Format.str_formatter
               "new name %s %a\n" n.txt Location.print n.loc) acc;
           failwith (Format.flush_str_formatter ())
         end
     in
     (new_name, acc)                       (* don't bother removing from acc*)
   end
   else match acc with
   | h::t -> (h,t)
   | _    -> assert false
 in
 let (desc,acc) = match pat.ppat_desc with
  | Ppat_any as x -> (x,acc)
  | Ppat_var old_name ->
      let (new_name,acc) = subst old_name acc in (Ppat_var new_name,acc)
  | Ppat_alias (p,old_name) ->
     let (p,acc) = pattern_subst ~by_name acc p in
     let (new_name,acc) = subst old_name acc in
     (Ppat_alias (p,new_name),acc)
  | Ppat_constant _ as x -> (x,acc)
  | Ppat_tuple pl ->
      let (pl,acc) = map_accum (pattern_subst ~by_name) acc pl in
      (Ppat_tuple pl,acc)
  | Ppat_construct (_,None) as x -> (x,acc)
  | Ppat_construct (lid,Some p) ->
     let (p,acc) = pattern_subst ~by_name acc p in
     (Ppat_construct (lid,Some p),acc)
  | Ppat_variant (_,None) as x -> (x,acc)
  | Ppat_variant (l,Some p) ->
     let (p,acc) = pattern_subst ~by_name acc p in
     (Ppat_variant (l,Some p),acc)
  | Ppat_record (pl,cf) ->
      let (pl,acc) = map_accum (fun acc (l,p) ->
          let (p,acc) = pattern_subst ~by_name acc p in ((l,p),acc)) acc pl in
      (Ppat_record (pl,cf),acc)
  | Ppat_array pl ->
      let (pl,acc) = map_accum (pattern_subst ~by_name) acc pl in
      (Ppat_array pl,acc)
  | Ppat_or (p1,p2) ->
     let (p1,acc') = pattern_subst ~by_name acc p1 in
     let (p2,_)   = pattern_subst ~by_name:true acc p2 in
     (Ppat_or (p1,p2), acc')
  | Ppat_constraint (p,cty) ->
     let (p,acc) = pattern_subst ~by_name acc p in
     (Ppat_constraint (p,cty), acc)
  | Ppat_type _ as x -> (x,acc)
  | Ppat_lazy p ->
     let (p,acc) = pattern_subst ~by_name acc p in
     (Ppat_lazy p, acc)
  | Ppat_unpack _ as x -> (x,acc)
  | _ -> assert false (* we do not create other forms of Ppat *)
 in
 ({pat with ppat_desc = desc}, acc)

let pattern_subst_list :
    string loc list -> Parsetree.pattern list ->
     Parsetree.pattern list * string loc list = fun acc pl ->
 map_accum (pattern_subst ~by_name:false) acc pl

(* Build the fresh variable name for cases and build the Parsetree
   case list
   We implicitly assume that all variables bound by patterns in any clause
   scopes over all clauses. That seems like a wild assumption: for example,
   in
     function | x -> e1 | y -> e2
   the variable x should scope only over e1 rather than also over e2.
   However, this wild scoping is no problem: recall that we process
   the Typedtree, and the type checker already determined the scoping.
   In the type-checked example
     let x = 1 in
     function | x -> e1 | y -> x + 2
   the variables are represented not just by their names but by their Path,
   which contains the unique timestamp. Therefore, we are actually dealing with
     let x/1 = 1 in
     function | x/2 -> e1 | y/3 -> x/1 + 2
   Therefore, if we make x/2 also scope over the second clause, that is
   harmless.
   Because of such scoping rules, prepare_cases is also useful
   for processing letrec.
*)
let prepare_cases : Location.t ->
  string loc heap ->     (* extra free variables used in continuation *)
  (* The following argument is a pair: a pattern list for the clauses
     of the function, and the list of names of bound variables, in order.
  *)
  (Parsetree.pattern list * string loc list) ->
  (* The following function returns the list of pairs of guards and bodies,
     for each clause of the function
   *)
  (expr_repr list -> (expr_repr option * expr_repr) list) ->
  (* The continuation *)
  (Parsetree.case list -> Parsetree.expression) -> expr_repr =
  fun loc evars (pats,old_names) fgbodies k ->
    let tr loc (eo,e) =
        let (eo,vo)        = validate_vars_option loc eo in
        let Code (vars,e)  = validate_vars loc e in
        ((eo,e),merge vo vars) in
    let (names,vars,egbodies) =
         with_binding_region_gen loc old_names tr fgbodies in
    let pats =
      if names = [] then pats else
      let (pats,acc) = pattern_subst_list names pats in
      assert (acc = []); pats
    in
    Code(merge evars vars,
         k @@ List.map2 (fun p (eo,e) -> {pc_lhs=p;pc_guard=eo;pc_rhs=e})
              pats egbodies)

module Vb = struct
  let mk loc p e =
    {pvb_pat = p;pvb_expr = e;pvb_loc = loc;pvb_attributes = [];}
end

(* Exression builders *)
module Expr = struct

  let mk loc d =
    {pexp_desc = d; pexp_loc = loc; pexp_attributes = []}

  let let_ ?loc ?attrs a b c = mk ?loc ?attrs (Pexp_let (a, b, c))


  let let_simple loc name e fbody = mk ?loc ?attrs (Pexp_let (a, b, c))
    let (name, vars1, ebody) = with_binding_region loc old_name fbody in
    let (vars2, e) = validate_vars loc e in
    let pat = Pat.var name.loc name in
    let vb = Vb.mk loc pat e in
    (merge vars1 vars2, mk loc Pexp_let (Nonrecursive, [vb], ebody))

  let fun_nonbinding loc label pat body =
    let (vars, c) = validate_vars loc c in
    (vars, mk loc (Pexp_fun (label, None, pat, body))

  let fun_simple loc label name fbody =
    let (name, vars, ebody) = with_binding_region loc name fbody in
    let pat = Pat.var name.loc name in
    (vars, mk loc (Pexp_fun (label, None, pat, ebody))

  let fun_ loc label pat names fbody =
    let (names, vars, ebody) = with_binding_regions loc names fbody in
    let pat, _ = pattern_subst names pat in
    (vars, (Pexp_function(label, None, pat, ebody)))

  let function_nonbinding loc pats exps =
    let cases = List.combine pats exps in
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

(* Build the general let-Parsetree (like the fun-Parsetree) *)
let build_let :
  Location.t -> bool ->
  (Parsetree.pattern list * string loc list) ->
  (expr_repr array -> (expr_repr option * expr_repr) array) -> expr_repr =
  fun loc recf pon fgbodies ->
  prepare_cases loc Nil pon fgbodies @@ function
    | [] | [_] -> assert false
      (* The first case is the pseudo-case for the let body *)
    | {pc_guard=None; pc_rhs=ebody} :: cases ->
        Ast_helper.Exp.let_ ~loc (if recf then Recursive else Nonrecursive)
          (List.map (function
            | {pc_lhs;pc_guard=None;pc_rhs} ->
                Ast_helper.Vb.mk ~loc:pc_lhs.ppat_loc pc_lhs pc_rhs
            | _ -> assert false)
           cases)
         ebody
    | _ -> assert false

(* build match and try: both are very similar and similar to build_fun *)
let build_match :
  Location.t -> (Parsetree.pattern list * string loc list) -> expr_repr ->
  int ->
  (expr_repr array -> (expr_repr option * expr_repr) array) -> expr_repr =
  fun loc pon ec nregular fgbodies ->
    let Code (evars,exp) = validate_vars loc ec in
    let split : int -> 'a list -> 'a list * 'a list = fun n lst ->
      let rec loop n acc lst = match (n,lst) with
      | (0,lst)  -> (List.rev acc,lst)
      | (n,h::t) -> loop (n-1) (h::acc) t
      | _        -> assert false
      in loop n [] lst
    in
    prepare_cases loc evars pon fgbodies @@ fun cases ->
      Ast_helper.Exp.match_ ~loc exp
      (let (rc,ec) = split nregular cases in
       rc @ List.map
          (fun c ->
            let pat = {c.pc_lhs with ppat_desc = Ppat_exception c.pc_lhs}
            in {c with pc_lhs = pat}) ec)


(* Essentially the same as build_match.
   TODO: implement the same check on the timestamp of the expression to try
*)
let build_try :
  Location.t -> (Parsetree.pattern list * string loc list) -> expr_repr ->
  (expr_repr array -> (expr_repr option * expr_repr) array) -> expr_repr =
  fun loc pon ec fgbodies ->
    let Code (evars,exp) = validate_vars loc ec in
    prepare_cases loc evars pon fgbodies @@ fun cases ->
      Ast_helper.Exp.try_ ~loc exp cases


let dyn_fail _ = failwith "CSP on local identifier"
