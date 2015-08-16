
(* ------------------------------------------------------------------------ *)

(* Stack marks, a simple form of dynamic binding *)

(* In the earlier version, our stackmarks could be ordered.
   Alas, it is hard to dynamically replace the implementation
   below with the one adjusted for delimcc. The implementation below
   does not work when partial continuations can be captured and reinstated.
   Mainly, when delimited continuations are used, the order is
   not stable. Delimited control operators can reshuffle the order
   arbitrarily. Therefore, the fact that there is order among valid stackmarks
   is not helpful anyway.

module type STACKMARK = sig
  type t
  val is_valid : t -> bool
  (* compare is supposed to be called on stack marks that are
     checked to be valid
   *)
  val compare : t -> t -> int
  val with_stack_mark : (t -> 'w) -> 'w
end

(* Simple implementation with shallow dynamic binding *)
module StackMark : STACKMARK = struct
  type t = int ref

  (* The global counter of the nesting depth of with_stack_mark *)
  let stack_mark_cnt = ref 0

  (* A stack mark is ref n where n is the depth of the corresponding
     with_stack_mark form.
     The stack mark is invalid if the counter is 0
   *)
  let with_stack_mark body =
    incr stack_mark_cnt;
    let mark = ref !stack_mark_cnt in
    let finalize () =
      mark := 0;                         (* invalidate the mark *)
      assert (!stack_mark_cnt > 0);
      decr stack_mark_cnt
    in
    try
      let r = body mark in finalize (); r
    with e -> finalize (); raise e

  let is_valid mark = !mark > 0
  let compare m1 m2 =
    assert (!m1 >0 && !m2 > 0);
    compare !m1 !m2
end

*)

(* A robust and truly minimalistic implementation of stack-marks.
   A stack-mark is created by 'with_stack_mark' function. Since
   the only operation on a stackmark is to test if it is valid,
   the stackmark is realized as a thunk unit -> bool.
*)
type stackmark = unit -> bool           (* true if valid *)

(* The type of the with_stack_mark operation *)
type stackmark_region_fn =
    {stackmark_region_fn : 'w. (stackmark -> 'w) -> 'w}

(* The simple implementation of stackmark_region_fn, appropriate
   when no delimited control is used.
   The mark is a ref bool cell, containing true within
   stackmark_region_fn's dynamic region.
*)
let with_stack_mark_simple : stackmark_region_fn =
  {stackmark_region_fn = fun body ->
    let mark = ref true in
    try
      let r = body (fun () -> !mark) in
      mark := false;                      (* invalidate the mark *)
      r
    with e -> mark := false; raise e
 }

let with_stack_mark : stackmark_region_fn ref = ref with_stack_mark_simple

(* Replace a with_stack_mark implementation, e.g., when delimcc is used *)
let set_with_stack_mark : stackmark_region_fn -> unit =
  fun smf -> with_stack_mark := smf

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

type prio = int
type 'v heap = Nil | HNode of prio * stackmark * 'v * 'v heap * 'v heap
let empty = Nil

let rec merge : 'v heap -> 'v heap -> 'v heap = fun h1 h2 ->
  match (h1,h2) with
  | (Nil,h) | (h,Nil)-> h
  | (HNode (p1,k1,v1,l1,r1), HNode (p2,k2,v2,l2,r2)) ->
      begin
        match p1 - p2 with
        | 0 -> HNode (p1,k1,v1, merge l1 l2, merge r1 r2) (* same keys *)
        | n when n < 0 -> HNode (p2,k2,v2, merge h1 l2, r2)
        | _ -> HNode (p1,k1,v1,l1,merge h2 r1)
      end

(* Remove the node with a given priority *)
let rec remove : prio -> 'v heap -> 'v heap = fun p -> function
  | Nil -> Nil
  | HNode (pn,k,v,h1,h2) as h ->
      begin
        match p - pn with
        | 0 -> merge h1 h2              (* p cannot occur in h1 or h2 *)
        | n when n > 0 -> h             (* entire tree has the lower prio *)
        | _ -> HNode (pn,k,v, remove p h1, remove p h2)
      end

(* ------------------------------------------------------------------------ *)


(* The representation of the possibly open code: AST plus the
   set of free identifiers, annotated with the marks
   of the corresponding with_binding_region forms
*)
type code_repr = Code of string loc heap * Parsetree.expression

(* The closed code is AST *)
type closed_code_repr = Parsetree.expression

(* Check that the code is closed and return the closed code *)

(* The same as close_code but return the closedness check as a thunk
   rather than performing it.
   This is useful for debugging and for showing the code
*)
let close_code_delay_check : code_repr -> closed_code_repr * (unit -> unit) =
 function
  | Code (Nil,ast) -> (ast,fun () -> ())
  | Code (HNode (_,_,var,_,_),ast) ->
    (ast, fun () ->
      Format.fprintf Format.str_formatter
      "The code built at %a is not closed: identifier %s bound at %a is free"
      Location.print ast.pexp_loc var.txt Location.print var.loc;
      failwith (Format.flush_str_formatter ()))

let close_code_repr : code_repr -> closed_code_repr = fun cde ->
  let (ast, check) = close_code_delay_check cde in
  check (); ast

let open_code : closed_code_repr -> code_repr = fun ast ->
  Code (Nil,ast)

(* Bindings in the future stage *)
(* Recall, all bindings at the future stage are introduced by
   patterns, and hence are simple names, without any module qualifications.
*)
let gensym_count = ref 0

(* Generate a fresh name with a given base name *)
let gensym : string -> string = fun s ->
  incr gensym_count;
  s ^ "_" ^ string_of_int !gensym_count

let reset_gensym_counter () = gensym_count := 0

(* Make a simple identifier unique *)
let genident : string loc -> string loc = fun name ->
  {name with txt = gensym name.txt}

(* left-to-right accumulating map *)
let rec map_accum : ('accum -> 'a -> 'b * 'accum) -> 'accum -> 'a list ->
  'b list * 'accum = fun f acc -> function
    | []   -> ([],acc)
    | h::t ->
        let (h,acc) = f acc h in
        let (t,acc) = map_accum f acc t in
        (h::t, acc)

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
let validate_vars : Location.t -> code_repr -> code_repr =
  fun l -> function
  | Code (Nil,_) as cde -> cde
  | Code (h, ast) as cde -> begin
      let rec check = function
        | Nil -> ()
        | HNode (_,sm,var,h1,h2) ->
            if sm () then (check h1; check h2)
            else scope_extrusion_error ~detected:l ~occurred:ast.pexp_loc var
      in check h; cde
  end

let validate_vars_option : Location.t -> code_repr option ->
  Parsetree.expression option * string loc heap =
  fun l -> function
  | None -> (None,Nil)
  | Some e -> let Code (vars, e) = validate_vars l e in (Some e, vars)

let validate_vars_map : Location.t ->
  (Location.t -> 'a -> 'b * string loc heap) -> 'a list ->
  'b list * string loc heap = fun loc f xs ->
  map_accum (fun acc x ->
      let (y,vars) = f loc x in
      (y, merge vars acc))
    Nil xs

let validate_vars_list : Location.t -> code_repr list ->
  Parsetree.expression list * string loc heap = fun l cs ->
  validate_vars_map l
      (fun l c -> let Code (vars,e) = validate_vars l c in (e,vars)) cs

(* Generate a fresh name off the given name, enter a new binding region
   and evaluate a function passing it the generated name as code_repr.
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
let prio_counter = ref 0

let with_binding_region :
  Location.t -> string loc -> (code_repr -> code_repr) ->
  string loc * string loc heap * Parsetree.expression = fun l name f ->
  let new_name = genident name in
  let (vars,e) =
   !with_stack_mark.stackmark_region_fn (fun mark ->
     incr prio_counter;
     let prio = !prio_counter in
     let var_code = (* code that corresponds to the bound variable *)
       Code (HNode (prio,mark,new_name,Nil,Nil),
          Ast_helper.Exp.mk ~loc:name.loc   (* the loc of the binder *)
           (Pexp_ident (mkloc (Longident.Lident new_name.txt) new_name.loc))) in
     let Code (vars,e) = validate_vars l (f var_code) in
     (remove prio vars, e)) in
  (new_name, vars, e)

(* The most general version with several bindings and several expressions
   that use the bindings
 *)
let with_binding_region_gen :
  Location.t -> string loc list ->
  (Location.t -> 'a -> 'b * string loc heap) -> (code_repr array -> 'a array) ->
  string loc list * string loc heap * 'b list
  = fun l names tr f ->
  let new_names = List.map genident names in
  let (vars,es) =
   !with_stack_mark.stackmark_region_fn (fun mark ->
     incr prio_counter;
     let prio = !prio_counter in
     let vars_code = Array.of_list (List.map (fun new_name ->
                      (* code that corresponds to a bound variable *)
       Code (HNode (prio,mark,new_name,Nil,Nil),
          Ast_helper.Exp.mk ~loc:new_name.loc    (* the loc of the binder *)
            (Pexp_ident (mkloc (Longident.Lident new_name.txt) new_name.loc))))
       new_names) in
     let cs = Array.to_list (f vars_code) in
     let (es,vars) = map_accum (fun vars c ->
                      let (e,var) = tr l c in
                      (e,merge var vars)) Nil cs in
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


(* Handle timestamp for builders of the type
      Parsetree.expression -> Parsetree.expression
*)
let code_wrapper :
    (Location.t -> Parsetree.expression -> Parsetree.expression) ->
    (Location.t -> code_repr -> code_repr) =
fun f l e ->
  let Code (vars,e) = validate_vars l e in
  Code (vars, f l e)

(* building a typical Parsetree node: Pexp_assert of expression*)
let build_assert : Location.t -> code_repr -> code_repr =
  code_wrapper
  (fun loc e -> Ast_helper.Exp.assert_ ~loc e)

(* When we translate the typed-tree, we have to manually compile
   the above code
First, to see the AST for the phrase, invoke the top-level with the flag
-dparsetree. Then
   {pexp_loc  = l; pexp_desc = Pexp_assert e}

gives the parsetree:
let build_assert_ast : Location.t -> Parsetree.expression -> Parsetree.expression =
{pexp_loc = l1;
 pexp_desc =
  Pexp_record
        ([(Location.mknoloc (Longident.parse "Parsetree.pexp_loc"),
           Pexp_ident "l");
         (Location.mknoloc (Longident.parse "Parsetree.pexp_desc"),
           {pexp_loc  = Location.none;
            pexp_desc = Pexp_construct
                          ((Location.mknoloc (Longident.parse
                                                "Parsetree.Pexp_assert")),
              Some {pexp_loc = Location.none;
                    pexp_desc = Pexp_ident "e"},
              false)})
        ],
        None)}
type_expression

If building the parsetree on our own, beware! For example, labels in
Texp_record must be sorted, in their declared order!
*)


(* Other similar builders *)
let build_lazy : Location.t -> code_repr -> code_repr =
  code_wrapper @@
    fun loc e -> Ast_helper.Exp.lazy_ ~loc e
let build_quote : Location.t -> code_repr -> code_repr =
  code_wrapper @@
    fun loc e -> Ast_helper.Exp.quote ~loc e
let build_escape : Location.t -> code_repr -> code_repr =
  code_wrapper @@
    fun loc e -> Ast_helper.Exp.escape ~loc e

let build_sequence : Location.t -> code_repr -> code_repr -> code_repr =
  fun loc e1 e2 ->
    let Code (vars1,e1) = validate_vars loc e1 in
    let Code (vars2,e2) = validate_vars loc e2 in
    Code (merge vars1 vars2,
          Ast_helper.Exp.sequence ~loc e1 e2)
let build_while : Location.t -> code_repr -> code_repr -> code_repr =
  fun loc e1 e2 ->
    let Code (vars1,e1) = validate_vars loc e1 in
    let Code (vars2,e2) = validate_vars loc e2 in
    Code (merge vars1 vars2,
          Ast_helper.Exp.while_ ~loc e1 e2)

(* Build the application. The first element in the array is the
   function. The others are arguments. *)
let build_apply : Location.t -> (label * code_repr) array -> code_repr =
  fun loc ea ->
    assert (Array.length ea > 1);
    match map_accum (fun vars (lbl,e) ->
                   let Code (var,e) = validate_vars loc e in
                   ((lbl,e),merge var vars))
          Nil (Array.to_list ea) with
    | (("",eh)::elt,vars) ->
       Code (vars,
             Ast_helper.Exp.apply ~loc eh elt)
    | _ -> assert false


let build_tuple : Location.t -> code_repr array -> code_repr =
 fun loc ea ->
  let (els,vars) = validate_vars_list loc (Array.to_list ea) in
  Code (vars,
        Ast_helper.Exp.tuple ~loc els)

let build_array : Location.t -> code_repr array -> code_repr =
 fun loc ea ->
  let (els,vars) = validate_vars_list loc (Array.to_list ea) in
  Code (vars,
        Ast_helper.Exp.array ~loc els)

let build_ifthenelse :
 Location.t -> code_repr -> code_repr -> code_repr option -> code_repr =
 fun loc e1 e2 eo ->
    let Code (vars1,e1) = validate_vars loc e1 in
    let Code (vars2,e2) = validate_vars loc e2 in
    let (eo,varso)      = validate_vars_option loc eo in
    Code (merge vars1 (merge vars2 varso),
          Ast_helper.Exp.ifthenelse ~loc e1 e2 eo)

let build_construct :
 Location.t -> Longident.t loc -> code_repr array -> code_repr =
 fun loc lid args ->
  let (args,vars) = validate_vars_list loc (Array.to_list args) in
  Code (vars,
        Ast_helper.Exp.construct ~loc lid
          begin
            match args with
            | []  -> None
            | [x] -> Some x
            | xl  -> Some (Ast_helper.Exp.tuple ~loc xl)
          end)

let build_record : Location.t -> (Longident.t loc * code_repr) array ->
 code_repr option -> code_repr =
 fun loc lel eo ->
   let (lel,vars) = map_accum (fun vars (lbl,e) ->
                       let Code (var,e) = validate_vars loc e in
                       ((lbl,e),merge var vars))
        Nil (Array.to_list lel) in
   let (eo,varo) = validate_vars_option loc eo in
   Code (merge vars varo,
         Ast_helper.Exp.record ~loc lel eo)

let build_field : Location.t -> code_repr -> Longident.t loc -> code_repr =
 fun loc e lid ->
  let Code (vars,e) = validate_vars loc e in
  Code (vars,
        Ast_helper.Exp.field ~loc e lid)

let build_setfield :
 Location.t -> code_repr -> Longident.t loc -> code_repr -> code_repr =
 fun loc e1 lid e2 ->
  let Code (vars1,e1) = validate_vars loc e1 in
  let Code (vars2,e2) = validate_vars loc e2 in
  Code (merge vars1 vars2,
        Ast_helper.Exp.setfield ~loc e1 lid e2)

let build_variant : Location.t -> string -> code_repr option -> code_repr =
 fun loc l eo ->
  let (eo,vars) = validate_vars_option loc eo in
  Code (vars,
        Ast_helper.Exp.variant ~loc l eo)

let build_send : Location.t -> code_repr -> string -> code_repr =
 fun loc e l ->
  let Code (vars,e) = validate_vars loc e in
  Code (vars,
        Ast_helper.Exp.send ~loc e l)

let build_open :
 Location.t -> Longident.t loc -> override_flag -> code_repr -> code_repr =
 fun loc l ovf e ->
  let Code (vars,e) = validate_vars loc e in
  Code (vars,
        Ast_helper.Exp.open_ ~loc ovf l e)

(* Build a function with a non-binding pattern, such as fun () -> ... *)
let build_fun_nonbinding :
  Location.t -> string -> Parsetree.pattern list ->
  (code_repr option * code_repr) array -> code_repr =
  fun loc label pats gbodies ->
  let (egbodies,vars) =
    validate_vars_map loc
      (fun loc (eo,e) ->
        let (eo,vo)        = validate_vars_option loc eo in
        let Code (vars,e)  = validate_vars loc e in
        ((eo,e),merge vo vars))
      (Array.to_list gbodies) in
  Code (vars,
        match (egbodies,pats) with
        | ([(None,e)],[p]) ->
            Ast_helper.Exp.fun_ ~loc label None p e
        | _ when label="" ->
          Ast_helper.Exp.function_ ~loc
            (List.map2 (fun p (eo,e) -> {pc_lhs=p;pc_guard=eo;pc_rhs=e})
              pats egbodies)
        | _ -> assert false)

(* Build a Parsetree for a future-stage identifier
   It is always in scope of with_binding_region:
   Bound variables are always in scope of their binders;
   A well-typed code has no unbound variables.
let build_ident : Location.t -> string loc -> code_repr =
 fun loc l ->
  not_supported loc "vars not supported"
  Code (add_timestamp (Some l)
   {pexp_loc  = loc;
    pexp_desc = Pexp_ident (mkloc (Longident.Lident l.txt) l.loc)}
*)

(* Build a simple one-arg function, as described in the the title comments *)
(* 'name' is the name of the variable from Ppat_var of the fun x -> ...
   form. It is the real name with the location within the function pattern.
   Use name.loc to identify the binder in the source code.
*)
let build_fun_simple :
  Location.t -> string -> string loc -> (code_repr -> code_repr) -> code_repr =
  fun loc label old_name fbody ->
  let (name, vars, ebody) = with_binding_region loc old_name fbody in
  let pat = Ast_helper.Pat.var ~loc:name.loc name in
  Code (vars,
        Ast_helper.Exp.fun_ ~loc label None pat ebody)

let build_for :
  Location.t -> string loc -> code_repr -> code_repr ->
  bool -> (code_repr -> code_repr) -> code_repr =
  fun loc old_name elo ehi dir fbody ->
  let (name, varsb, ebody) = with_binding_region loc old_name fbody in
  let Code (varsl,elo) = validate_vars loc elo in
  let Code (varsh,ehi) = validate_vars loc ehi in
  Code (merge varsb (merge varsl varsh),
        Ast_helper.Exp.for_ ~loc
          (Ast_helper.Pat.var ~loc:name.loc name) elo ehi
            (if dir then Upto else Downto) ebody)


let build_let_simple_nonrec :
  Location.t -> string loc -> code_repr -> (code_repr -> code_repr) ->
    code_repr = fun loc old_name e fbody ->
  let (name, varsb, ebody) = with_binding_region loc old_name fbody in
  let pat = Ast_helper.Pat.var ~loc:name.loc name in
  let Code (varse,e) = validate_vars loc e in
  Code (merge varsb varse,
        Ast_helper.Exp.let_ ~loc Nonrecursive
           [Ast_helper.Vb.mk ~loc pat e] ebody)

(*
let build_letrec :
  Location.t -> string loc array ->
    (code_repr array -> code_repr array) -> code_repr =
  fun l old_names fbodies ->
  let (names,vars,ebodies) =
    with_binding_region_gen l (Array.to_list old_names) fbodies in
  let (ebody,es) =
    match ebodies with body::es -> (body,es) | _ -> assert false in
  let pel = List.map2 (fun name e ->
     ({ppat_loc  = name.loc; ppat_desc = Ppat_var name},e)) names es in
  Code (vars,
  {pexp_loc = l;
   pexp_desc = Pexp_let (Recursive, pel, ebody)})
*)

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



(* Build the general fun Parsetree *)

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
  string loc heap ->     (* extra free variables used in kontinuation *)
  (* The following argument is a pair: a pattern list for the clauses
     of the function, and the list of names of bound variables, in order.
  *)
  (Parsetree.pattern list * string loc list) ->
  (* The following function returns the list of pairs of guards and bodies,
     for each clause of the function
   *)
  (code_repr array -> (code_repr option * code_repr) array) ->
  (* The continuation *)
  (Parsetree.case list -> Parsetree.expression) -> code_repr =
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

let build_fun :
  Location.t -> string ->
  (Parsetree.pattern list * string loc list) ->
  (code_repr array -> (code_repr option * code_repr) array) -> code_repr =
  fun loc label pon fgbodies ->
  prepare_cases loc Nil pon fgbodies @@ function
    | [{pc_lhs=p; pc_guard=None; pc_rhs=e}] ->
        Ast_helper.Exp.fun_ ~loc label None p e
    | cases when label="" ->
        Ast_helper.Exp.function_ ~loc cases
    | _ -> assert false


(* Build the general let-Parsetree (like the fun-Parsetree) *)
let build_let :
  Location.t -> bool ->
  (Parsetree.pattern list * string loc list) ->
  (code_repr array -> (code_repr option * code_repr) array) -> code_repr =
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
  Location.t -> (Parsetree.pattern list * string loc list) -> code_repr ->
  int ->
  (code_repr array -> (code_repr option * code_repr) array) -> code_repr =
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
  Location.t -> (Parsetree.pattern list * string loc list) -> code_repr ->
  (code_repr array -> (code_repr option * code_repr) array) -> code_repr =
  fun loc pon ec fgbodies ->
    let Code (evars,exp) = validate_vars loc ec in
    prepare_cases loc evars pon fgbodies @@ fun cases ->
      Ast_helper.Exp.try_ ~loc exp cases


let dyn_fail _ = failwith "CSP on local identifier"
