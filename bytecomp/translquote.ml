open Parsetree
open Asttypes
open Typedtree
open Types

(* Emit a translation-time error *)
exception Error of Location.error

let trx_error loc_err = raise @@ Error loc_err

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some err
      | _         -> None
    )

let not_supported loc msg =
  trx_error @@ Location.errorf ~loc
      "%s is not yet supported within brackets" msg

(* left-to-right accumulating map *)
let rec map_accum : ('accum -> 'a -> 'b * 'accum) -> 'accum -> 'a list ->
  'b list * 'accum = fun f acc -> function
    | []   -> ([],acc)
    | h::t ->
        let (h,acc) = f acc h in
        let (t,acc) = map_accum f acc t in
        (h::t, acc)

let initial_env = Env.initial_safe_string

(* In a Typedtree, <e> is represented as a sequence
        begin 0; e end
   again, with the corresponding attribute.
   I chose 0 rather than () because if we forget to handle
   bracket/escape properly, we get a warning. Still, begin 0; e end
   is syntactically and type-correct .
   Ditto for Escape.
*)

(* Path and location utilities *)

(* ------------------------------------------------------------------------ *)
(* Path utilities *)

(* We always use path when available, and convert it to Longident
   when needed -- even if the Typedtree already carries the longident.
   The path is preferred because it is fully qualified for
   external identifiers and it is unambiguous.
   If we open a module, its components can be referred to without
   qualification -- the path will be qualified nevertheless.
   When we build a Parsetree representing the generated code,
   we have to use fully qualified identifiers since the open statement
   in the original code won't be represented in the generated
   Parsetree.
*)

(* Check to see if a path refers to an identifier, exception, or
   constructor that is available from an external module. If so, the run-time
   compiler invoked by run can get the definition for the identifier from
   a .cmi file. The value of an external identifier can be obtained from
   a .cmo file.
   If a path contains several components like
   M1.M2.M3.ident, we should check if the top-most component, that is, M1,
   is external.
*)
(* XXX call Env.normalize_path first? *)

let rec is_external = function
  | Path.Pident id ->           (* not qualified *)
      Ident.persistent id || Ident.global id || Ident.is_predef_exn id
  | Path.Papply _     -> false
  | Path.Pdot(p, _,_) -> is_external p

(* Convert a path to an identifier. Since the path is assumed to be
   `global', time stamps don't matter and we can use just strings.
*)
let rec path_to_lid : Path.t -> Longident.t = function
  | Path.Pident i       -> Longident.Lident (Ident.name i)
  | Path.Pdot (p,s,_)   -> Longident.Ldot (path_to_lid p, s)
  | Path.Papply (p1,p2) ->
      Longident.Lapply(path_to_lid p1, path_to_lid p2)

(* Convert the path to lid but use the given str as the last component.
   This in effect qualifies 'str' with the given module path
*)
let path_to_lid_but_last : Path.t -> string -> Longident.t =
  fun p str ->
    match p with
    | Path.Pident _ -> Longident.Lident str
    | Path.Pdot (p,_,pos) -> path_to_lid (Path.Pdot (p,str,pos))
    | _ -> assert false


(* Check to make sure a constructor, label, exception, etc.
   have the name that we can put into AST (Parsetree).
   Local names can't be put into the Parsetree since the type env in which
   they are declared is not represented in the Parsetree.
*)
let check_path_quotable msg path =
  if not (is_external path) then
    trx_error @@ Location.errorf
        "%s %s cannot be used within brackets. Put into a separate file."
        msg (Path.name path)


(* Check to see that a constructor belongs to a type defined
   in a persistent module or in the initial environment.
   Return the fully qualified name to put into AST
   (Pervasive constructors remain unqualified however).

   We have nothing to do if the constructor is already fully qualified
   with a persistent module identifier: for example, Scanf.Scan_failure.
   The major complexity comes from this scenario:
      open Scanf
      <<raise (Scan_failure "xx")>>
   The Texp_construct node of Typedtree contains the lid and (was: the
   path) that refer to "Scan_failure" without any module qualifications.
   We have to find the fully qualified path and check
   that it is external. We do that by finding the path for the _type_
   constructor, for the type of which the data constructor is a member.
   That type_path is fully qualified. We can ascertain the later fact
   from Env.constructors_of_type, which puts the complete path
   into the type of the constructor, which is always of the form
   Tconstr(ty_path,_,_). The function constructors_of_type is used
   within Env.store_type, which is used when opening a module.

   Alternatively we could've used Env.lookup_constructor, which also
   returns the qualified path? Searching the environment is costly
   though.
   Actually, using Env.lookup_constructor is a bad idea. Now labels and
   constructors don;t have to be unique. The type checker goes to
   a great length to disambiguate a constructor or a label. It records
   the eventually determined type of the label/constructor in
   label_description or constructor_description.
   So, we should only use information from these descriptions.

   Alas, the predefined types (with no module qualification) are
   not specially distinguished. So, we have to check the initial
   environment.
 *)
let qualify_ctor :
    Longident.t loc -> constructor_description -> Longident.t loc =
 fun lid cdesc ->
  let loc = lid.loc in
  match (cdesc.cstr_tag, Ctype.repr cdesc.cstr_res) with
  | (Cstr_extension (p,_),_) ->
      if is_external p then Location.mkloc (path_to_lid p) loc else
       trx_error @@ Location.errorf ~loc
       "Exception (extension) %s cannot be used within brackets. Put into a separate file."
        (Path.name p)
  | (_,{desc = Tconstr((Path.Pident _ as ty_path), _, _)}) ->
     begin
      try ignore (Env.find_type ty_path initial_env); lid
      with Not_found ->
        trx_error @@ Location.errorf ~loc
        "Unqualified constructor %s cannot be used within brackets. Put into a separate file."
          cdesc.cstr_name
     end
  | (_,{desc = Tconstr(ty_path, _, _)}) ->
      if is_external ty_path then
        Location.mkloc (path_to_lid_but_last ty_path cdesc.cstr_name) loc
      else
      trx_error @@ Location.errorf ~loc
      "Constructor %s cannot be used within brackets. Put into a separate file."
          cdesc.cstr_name
  | _ -> Printtyp.type_expr Format.err_formatter cdesc.cstr_res;
           failwith ("qualify_ctor: cannot determine type_ctor from data_ctor "^
                     cdesc.cstr_name)

(* Check to see that a record label belongs to a record defined
   in a persistent module or in the initial environment.
   This is a label version of qualify_ctor
*)
let qualify_label : Longident.t loc -> label_description -> Longident.t loc =
 fun lid ldesc ->
  let loc = lid.loc in
  match Ctype.repr ldesc.lbl_res with
  | {desc = Tconstr((Path.Pident _ as ty_path), _, _)} ->
    begin
      try ignore (Env.find_type ty_path initial_env); lid
      with Not_found ->
        trx_error @@ Location.errorf ~loc
        "Unqualified label %s cannot be used within brackets. Put into a separate file."
          ldesc.lbl_name
    end
  | {desc = Tconstr(ty_path, _, _)} ->
      if is_external ty_path then
        Location.mkloc
          (path_to_lid_but_last ty_path ldesc.lbl_name) loc
      else
        trx_error @@ Location.errorf ~loc
          "Label %s cannot be used within brackets. Put into a separate file."
          ldesc.lbl_name
  | _ -> Printtyp.type_expr Format.err_formatter ldesc.lbl_res;
           failwith ("qualify_label: cannot determine type from label "^
                     ldesc.lbl_name)

(* Test if we should refer to a CSP value by name rather than by
   value
*)
(* Module identifiers for the modules that are expected to be
   present at run-time -- that is, will be available for
   dynamic linking of the run-time generated code.

TODO: check bytecomp/transclass.ml:const_path
Perhaps that's a hint which unqualified identifiers will be persistent
*)

let ident_can_be_quoted = is_external

let texp_int : int -> Typedtree.expression = fun n ->
  mk_texp ~env:Env.initial_safe_string (Texp_constant (Const_int n))
    (Ctype.instance_def Predef.type_int)

(* Make a bracket or an escape node
   Here, the attr argument is a bracket/escape attribute
*)
let texp_zero = (* TExp node for constant 0 *)
  texp_int 0

let texp_braesc :
  attribute -> Typedtree.expression -> Env.t -> type_expr ->
  Typedtree.expression =
  fun attr exp env ty ->
    mk_texp ~env ~attrs:(attr :: exp.exp_attributes)
            ~loc:exp.exp_loc (Texp_sequence (texp_zero, exp)) ty


(* TODO: add memoization? *)

(* Compiling an identifier with a given (qualified) name *)
let texp_ident : string -> expression = fun name ->
  let lid     = Longident.parse name in
  let (p, vd) = try Env.lookup_value lid initial_env
                with Not_found ->
                  Misc.fatal_error ("Trx.find_value: " ^ name) in
  mk_texp (Texp_ident (p,mknoloc lid, vd))
          (Ctype.instance initial_env vd.val_type)


(* Building an application *)
let texp_apply : Typedtree.expression -> Typedtree.expression list ->
 Typedtree.expression_desc = fun f args ->
   Texp_apply(f, List.map (fun arg -> ("",Some arg, Required)) args)

(* Compiling a string constant *)
(* The second argument of Const_string is the delimiter,
   the decorator in the {decorator| ... |decorator} notation.
*)
let texp_string : string -> Typedtree.expression = fun str ->
  mk_texp (Texp_constant (Const_string (str,None)))
          (Ctype.instance_def Predef.type_string)

(* Compiling a boolean *)
(* For prototype, see Typecore.option_none *)
let texp_bool : bool -> Typedtree.expression = fun b ->
  let lid = Longident.Lident (if b then "true" else "false") in
  let cdec = Env.lookup_constructor lid initial_env in
  mk_texp (Texp_construct(mknoloc lid, cdec, []))
          (Ctype.instance_def Predef.type_bool)

(* Given a value v, create a Typedtree node for an expression
   that will evaluate to v.
   This the the CSP used by the MetaOCaml itself.
   Since this is an internal CSP, we don't put any attributes.
*)
let texp_csp : Obj.t -> Typedtree.expression = fun v ->
  if Obj.is_int v then texp_int (Obj.obj v)
   (* We treat strings and bytes identically *)
  else if Obj.tag v = Obj.string_tag then texp_string (Obj.obj v)
  else
    let vstr = Marshal.to_string v [] in
    mk_texp (texp_apply (texp_ident "Marshal.from_string")
                        [texp_string vstr; texp_zero])
            (Btype.newgenvar ())


(* Compiling location data *)
(* We could have made texp_loc an alias to texp_csp... We keep the
   type information for location though, just to be fully correct.
*)
let texp_loc : Location.t -> Typedtree.expression = fun loc ->
  let loc_exp = texp_ident "Trx.loc_none" in (* this fills in the type, etc.*)
  if loc == Location.none then loc_exp else
  {loc_exp with exp_desc = (texp_csp (Obj.repr loc)).exp_desc}

(* Compiling longident with location data *)
let texp_lid : Longident.t loc -> Typedtree.expression = fun lid ->
  let lid_exp = texp_ident "Trx.sample_lid" in (* this fills in the type, etc.*)
  {lid_exp with exp_desc = (texp_csp (Obj.repr lid)).exp_desc}

(* Compiling a string with a location *)
let texp_string_loc : string loc -> Typedtree.expression = fun name ->
  let name_exp = texp_ident "Trx.sample_name" in
  {name_exp with
   exp_desc = (texp_csp (Obj.repr name)).exp_desc}

(* Compiling an option *)
(* For prototype, see Typecore.option_none *)
let texp_option : Typedtree.expression option -> Typedtree.expression =
  function
    | None ->
        let lid = Longident.Lident "None" in
        let cnone = Env.lookup_constructor lid initial_env in
        mk_texp (Texp_construct(mknoloc lid, cnone, []))
                (Ctype.instance_def (Predef.type_option (Btype.newgenvar ())))
    | Some e ->
        let lid = Longident.Lident "Some" in
        let csome = Env.lookup_constructor lid initial_env in
        mk_texp (Texp_construct(mknoloc lid, csome, [e]))
                (Ctype.instance_def (Predef.type_option e.exp_type))
                ~env:e.exp_env

(* Compiling a tuple *)
let texp_tuple : Typedtree.expression list -> Typedtree.expression = fun el ->
  mk_texp (Texp_tuple el)
          (Ctype.newty (Ttuple (List.map (fun e -> e.exp_type) el)))

(* Compiling an array *)
(* We use this function for grouping trx_bracket-transformed expressions,
   which have the same representation type (but may be different
   code type). We ignore the differences in the code type, since
   the representation type is the same.

   We don't use lists since they are harder to compile, and more
   fragile. Texp_construct has more arguments, we have to locate
   constructor information, etc.
*)
let texp_array : Typedtree.expression list -> Typedtree.expression = function
  | [] ->
      mk_texp (Texp_array [])
	      (Ctype.instance_def (Predef.type_array (Btype.newgenvar ())))
  | (h::_) as el ->
      mk_texp (Texp_array el)
	      (Ctype.instance_def (Predef.type_array h.exp_type))

(* Compiling patterns and the list of names bound by them *)
let texp_pats_names : Parsetree.pattern list -> string loc list ->
  Typedtree.expression = fun pats names ->
    let pn_exp = texp_ident "Trx.sample_pats_names" in
    {pn_exp with
     exp_desc = (texp_csp (Obj.repr (pats,names))).exp_desc}

(* Utility function to build the case list *)
let texp_case : ?guard:expression -> pattern -> expression -> case =
  fun ?guard pat exp ->
    {c_lhs=pat; c_guard=guard; c_rhs=exp}

(* Compiling a closed code value: a structural constant of
   type code_repr
   This constant is transported via CSP (although we could have
   built a Typedtree node for that purpose.
 *)
let texp_code : ?node_id:string ->
  Location.t -> Parsetree.expression_desc -> Typedtree.expression_desc =
  fun ?(node_id="") loc desc ->
  let ast = Ast_helper.Exp.mk ~loc desc in
    texp_apply (texp_ident "Trx.open_expr")
               [texp_csp (Obj.repr ast)]


(* ------------------------------------------------------------------------ *)
(* Dealing with CSP *)

(* Build the Typedtree that lifts the variable with the given path and type.
   Since this code receives the type of the variable, we use the
   type to generate the lifting code for that particular type.
   For example, we build the code to convert a float
   0.1 to the Parsetree node Pexp_constant(Const_float "0.1")).
   If we cannot or would not do the type-dependent lifting and we cannot
   refer to the variable by name (e.g., because it is local),
   we generate the call to the dynamic quoter, dyn_quote.
   The latter will receive the actual value to quote and will generate,
   at run-time, a Parsetree constant or CSP, based on that value.
 *)
let trx_csp :
  Typedtree.expression -> Path.t -> Longident.t loc ->
  Typedtree.expression_desc = fun exp p li ->
  (* Then check if we can pass by reference *)
  if ident_can_be_quoted p then
    texp_code ~node_id:"*id*" exp.exp_loc
          (Pexp_ident (Location.mkloc (path_to_lid p) li.loc))
  else  (* Otherwise, fail at run-time *)
  texp_apply (texp_ident "Trx.dyn_fail") [texp_int 0]

(* Translating patterns and expressions using patterns *)

(* Analyze and translate a pattern:
         Typedtree.pattern -> Parsetree.pattern
  The function is somewhat similar to tools/untypeast.ml:untype_pattern

  However, we also determine and return the list of bound variables.
  The list is in the reverse of the order of variables occurring in the pattern.
  Finally, we check that labels and constructors may be quoted.

  The algorithm of determining the names of bound variables is based
  on Typedtree.pat_bound_idents. There is one subtle issue.
  Normally all variables within a pattern are unique (patterns are
  always linear). Identically named variables within a list of patterns, like
      match ... with
      | [x] ->
      | [x;y] ->
  are _distinct_ variables. They have different Ident.t values, even though
  their names may be the same. However, components of an OR pattern
  bind exactly the same identifiers. Don't count them twice!
*)


(* The first argument is a list of identifiers. Found identifiers are
   prepended to that list. The order of identifiers is important!
   If you change the traversal order, be sure to modify pattern_subst below!
*)
let rec trx_pattern :
    (Ident.t * string loc) list -> Typedtree.pattern ->
     Parsetree.pattern * (Ident.t * string loc) list = fun acc pat ->
  let (pd,acc) = match pat with
  |  { pat_extra=[Tpat_unpack, _, _attrs]; pat_desc = Tpat_var (_,name); _ } ->
        (Ppat_unpack name,acc)          (* name must have been uppercase *)
  | { pat_extra=[Tpat_type (_path, lid), _, _attrs]; _ } -> (Ppat_type lid,acc)
  | { pat_extra= (Tpat_constraint ct, _, _attrs) :: rem; _ } ->
      not_supported pat.pat_loc
        "patterns with constraints, and other pat_extra";
      (*
        Ppat_constraint (untype_pattern { pat with pat_extra=rem },
                         untype_core_type ct)
       *)
  | _ -> match pat.pat_desc with
  | Tpat_any -> (Ppat_any, acc)
  | Tpat_var (id, name) when
      (match (Ident.name id).[0] with 'A'..'Z' -> true | _ -> false) ->
        (Ppat_unpack name,acc)        (* We don't handle modules though...*)
  | Tpat_var (id, name) ->
      (Ppat_var name, (id,name)::acc)
  | Tpat_alias (p, id, name) ->
      let (p,acc) = trx_pattern acc p in
      (Ppat_alias (p, name),(id,name)::acc)
  | Tpat_constant cst -> (Ppat_constant cst, acc)
  | Tpat_tuple lst ->
    let (pl,acc) = map_accum trx_pattern acc lst
    in (Ppat_tuple pl, acc)
  | Tpat_construct (li, cdesc, args) ->
      let lid = qualify_ctor li cdesc in
      let (args,acc) = map_accum trx_pattern acc args in
      (Ppat_construct (lid,
          (match args with
          | []  -> None
          | [x] -> Some x
          | _   -> Some (Ast_helper.Pat.tuple ~loc:pat.pat_loc args))),
       acc)
  | Tpat_variant (label, None, _) -> (Ppat_variant (label,None),acc)
  | Tpat_variant (label, Some p, _) ->
      let (p,acc) = trx_pattern acc p
      in (Ppat_variant (label,Some p),acc)
  | Tpat_record (lst, closed) ->
      let dolab acc (li,ldesc,pat) =
        let lid = qualify_label li ldesc in
        let (pat,acc) = trx_pattern acc pat in
        ((lid,pat),acc)
      in
      let (lpl,acc) = map_accum dolab acc lst in
      (Ppat_record (lpl,closed),acc)
  | Tpat_array lst ->
    let (pl,acc) = map_accum trx_pattern acc lst
    in (Ppat_array pl, acc)
  | Tpat_or (p1, p2, _) ->
      (* Invariant : both arguments bind the same variables *)
      let (p1,acc) = trx_pattern acc p1 in
      let (p2,_)   = trx_pattern acc p2 in (* ignore vars in p2 *)
      (Ppat_or (p1,p2),acc)
  | Tpat_lazy p ->
      let (p,acc) = trx_pattern acc p in (Ppat_lazy p,acc)
  in
  (Ast_helper.Pat.mk ~loc:pat.pat_loc ~attrs:pat.pat_attributes pd, acc)


(* Process all patterns in the case list *)
(* Patterns are processed left-to-right. The result is the processed
   pattern list plus the list of names of the bound variables.
   The variables are listed in the order they occur in the pattern.
   Thus the following should hold:
      let (pats,names,_) = trx_cl cl in
      let (pats',acc) =  pattern_subst_list names pats in
      assert (pats = pats');
      assert (acc = [])
   The final result of trx_cl is the pattern binding the names.
   We build an array pattern rather than a more appropriate tuple.
   Using array forces a single type to all arguments. Although
   it is phantom anyway, it is still a bummer. But with the tuple
   we can't generically write build_fun.
   The second argument, typ_expr, should normally be a code type.

   This function is used when translating a future-stage function as the
   present-stage whose argument is an array of variables.
   See trx_bracket for functions, let, match and try
*)
let trx_cl : case list -> type_expr ->
     Parsetree.pattern list * string loc list * Typedtree.pattern
   = fun cl typ ->
   let (pats, lst) =
     map_accum (fun acc {c_lhs} -> trx_pattern acc c_lhs) [] cl in
   let idnames = List.rev lst in
   let (loc,env) =
     match cl with {c_lhs=p}::_ -> (p.pat_loc, p.pat_env) |_ -> assert false in
    (* Pattern representing one binding variable *)
   let var_pat (id,name) =
    {pat_loc = loc; pat_extra = []; pat_env = env;
     pat_desc = Tpat_var (id,name);
     pat_attributes=[];
     pat_type = typ} in
   (pats, List.map snd idnames,
    {pat_loc = loc; pat_extra = []; pat_env = env;
     pat_attributes=[];
     pat_desc = Tpat_array (List.map var_pat idnames);
     pat_type = Ctype.instance_def (Predef.type_array typ)})

(* ------------------------------------------------------------------------ *)
(* The main function to translate away brackets. It receives
   an expression at the level n > 0.

   Since bracket-translation is somewhat similar to un-typechecking,
   see tools/untypeast.ml for hints on mapping Typedtree.expression
   to Parsetree.expression.

TODO: an optimization idea. Consider <assert e> as a typical expression.
We translate it to the invocation of build_assert that will construct
the Parsetree node at run-time. However, if 'e' is simple (e.g., a constant)
then we can construct the Parsetree node at compile time and pass it
as a CSP. There are no longer any functions calls to make at run-time.
So, we can modify the translation of <assert e> below to detect
if the translation of e produced Texp_cspval. We extract the CSP value,
invoke build_assert (at compile time, when trx.ml is run) to build
the Pexp_assert node, and wrap it as a CSP.

Essentially the result of trx_bracket should be like
   Transl_bracket of Parsetree.expression option * Typedtree.expression
The first part of the result is the code built-in at compile time.
This part is None of the expression to translate contains an escape
or a true CSP (global id is OK). Sometimes we need both parts: consider
       <fun x -> x + ~(...)>
When we translate x we don't know if we can take a shortcut and
build the function code at translation time. So, we have to account
for both possibilities. If we can build the function at compile time,
we don't even need to rename the bound variable!

*)

(* Given a type [ty], return [ty code code ... code] (n times code).
   When we push the bracket in, expressions that had type ty before
   will have the type ty code.
   Here, ty code is an abstract type whose concrete representation
   is code_repr.
   Generally speaking we don't have to adjust the types since the
   type checking is finished. However, code generator may look
   at types; it's better if we don't lie. Thus, as trx_bracket
   translates the expression, it should also adjust the types.
*)

let rec wrap_ty_in_code : int -> type_expr -> type_expr = fun n ty ->
  if n=0 then ty else
  (* let clsfier = Btype.newgenvar () in *)
  wrap_ty_in_code (n-1) (Predef.type_expr ty)

let map_option : ('a -> 'b) -> 'a option -> 'b option = fun f -> function
  | None   -> None
  | Some x -> Some (f x)


let rec trx_bracket_desc : int -> expression -> expression_desc = fun n exp ->
  match exp.exp_desc with
    (* Don't just do only for vd.val_kind = Val_reg
       because (+) or Array.get are Val_prim *)
  | Texp_ident (p,li,vd)  ->
    let stage = vd.val_stage in
    (* We make CSP only if the variable is bound at the stage 0.
       Variables bound at stage > 0 are subject to renaming.
       They are translated into stage 0 variable but of a different
       type (t code), as explained in the title comments.
     *)
    if stage = 0 then trx_csp exp p li
    else
         (* Future-stage bound variable becomes the present-stage
            bound-variable, but at a different type.
          *)
      let () = assert (vd.val_kind = Val_reg) in
      (* The drawback is that exp.exp_loc disappears. If the scope extrusion
         is reported for a simple expression like <x>, we can no longer
         print in the error message the location that <x> appeared.
         We can only print the location x was bound.
      *)
      Texp_ident (p,li,{vd with val_type = wrap_ty_in_code n vd.val_type})

  | Texp_constant cst ->
      texp_code ~node_id:"*cst*" exp.exp_loc (Pexp_constant cst)

     (* The most common case of let-expressions: non-recursive
        let x = e in body *)
  | Texp_let (Nonrecursive,[{vb_pat = {pat_desc = Tpat_var (_,name)} as pat;
                     vb_expr = e}],ebody) ->
      let pat = {pat with pat_type = wrap_ty_in_code n pat.pat_type} in
      texp_apply (texp_ident "Trx.build_let_simple_nonrec")
        [texp_loc exp.exp_loc;
         texp_string_loc name;
         trx_bracket n e;
         { exp with
           exp_desc =
             Texp_function ("",[texp_case pat (trx_bracket n ebody)],Total);
           exp_type =
            {exp.exp_type with desc =
               Tarrow ("",pat.pat_type, wrap_ty_in_code n ebody.exp_type, Cok)}
         }
       ]

     (* General case of let. There are two subcases: parallel and recursive:
          let     x = e1 and y = e2 ... in body
          let rec x = e1 and y = e2 ... in body

        The difference between them is profound: in the first case,
        x and y do not scope over e1 and e2, but in the recursive case,
        they do.
        And yet we translate the two cases uniformly. Recall that we are
        processing the Typedtree: the type checker already determined
        the scoping rules. The variables are represented with their paths,
        which bear unique timestamps. See the comment in prepare_cases
        above.
     *)
     (* Recursive let:
         let rec f = e1 [and g = e2 ...] in body
        According to transl_let in bytecomp/translcore.ml,
        the patterns in recursive let are very restrictive: elther
          let rec var = ...
        or
          let rec _ as var = ...
       For instance, let rec (x1,x2) = ... is not allowed.
       We do this test here. For simplicity, we are not going to support
          let rec _ as var = ...
       pattern.

       There is another constraint: see check_recursive_lambda in
       bytecomp/translcore.ml. We use a simpler version of the test:
       we allow only letrec expressions of the form
           let rec f = fun x -> ....
       that is,
           let rec f x y ... =
    *)
  | Texp_let (recf,vbl,ebody) ->
      let check_letrec ({vb_pat=p;vb_expr=e} as vb) =
        begin
        match p.pat_desc with
        | Tpat_var (_,_) -> ()
        | _ ->
            trx_error @@ Location.errorf ~loc:p.pat_loc
            "Only variables are allowed as left-hand side of `let rec'"
        end;
        match e.exp_desc with
        | Texp_function (_,_,_) -> ()
        | _ ->
            trx_error @@ Location.errorf ~loc:vb.vb_loc
            "Recursive let binding must be to a function"
            (* Location.print e.exp_loc *)
      in if recf = Recursive then
        List.iter check_letrec vbl;
      (* Artificially convert vbl to case list, making the body
         the first case with the Pat_any pattern.
         Of course the scoping rules are different for vbl and case list.
         Again, scoping has been already determined and resolved,
         and our case list processing assumes very wild scoping that
         accommodates let, letrec, functions, etc.
       *)
      let cl =
        {c_lhs =                        (* pseudo-pattern for ebody *)
          {pat_desc = Tpat_any; pat_loc=ebody.exp_loc;
           pat_attributes=[]; pat_extra=[];
           pat_type=ebody.exp_type;
           pat_env=ebody.exp_env};
         c_guard=None;
         c_rhs=ebody} ::
        List.map (fun {vb_pat;vb_expr} -> texp_case vb_pat vb_expr) vbl in
      let (pl,names,binding_pat) =
        trx_cl cl (wrap_ty_in_code n (Btype.newgenvar ())) in
      texp_apply (texp_ident "Trx.build_let")
        [texp_loc exp.exp_loc;
         texp_bool (recf = Recursive);
         texp_pats_names pl names;
         trx_case_list_body n binding_pat exp cl
       ]

     (* The most common case of functions: fun x -> body *)
  | Texp_function (l,[{c_guard=None;
                       c_lhs={pat_extra=[];
                              pat_desc = Tpat_var (_,name)} as pat;
                       c_rhs=ebody}],_) ->
      let pat = {pat with pat_type = wrap_ty_in_code n pat.pat_type} in
      texp_apply (texp_ident "Trx.build_fun_simple")
        [texp_loc exp.exp_loc;
         texp_string l;
         texp_string_loc name;
         (* Translate the future-stage function as present-stage function;
            with the same variables, but with a different type,
            targ code -> tres code
          *)
         { exp with
           exp_desc =
             Texp_function
               ("",[texp_case pat (trx_bracket n ebody)],Total);
           exp_type =
            {exp.exp_type with desc =
               Tarrow ("",pat.pat_type, wrap_ty_in_code n ebody.exp_type, Cok)}
         }
       ]

  | Texp_function (l,cl,_) ->
      begin
      match trx_cl cl (wrap_ty_in_code n (Btype.newgenvar ())) with
      | (pl, [], _) ->                    (* non-binding pattern *)
          texp_apply (texp_ident "Trx.build_fun_nonbinding")
            [texp_loc exp.exp_loc;
             texp_string l;
             begin
               let pl_exp = texp_ident "Trx.sample_pat_list" in
               {pl_exp with
                exp_desc = (texp_csp (Obj.repr pl)).exp_desc}
             end;
             texp_array (List.map (fun {c_guard;c_rhs;_} ->
                 texp_tuple [texp_option @@
                              map_option (trx_bracket n) c_guard;
                             trx_bracket n c_rhs])
                 cl)
           ]
      | (pl, names, binding_pat) ->
          texp_apply (texp_ident "Trx.build_fun")
            [texp_loc exp.exp_loc;
             texp_string l;
             texp_pats_names pl names;
             trx_case_list_body n binding_pat exp cl
           ]
      end

  | Texp_apply (e, el) ->
     (* first, we remove from el the information added by the type-checker *)
     let lel = List.fold_right (function                 (* keep the order! *)
                | (_,None,_)   -> fun acc -> acc
                | (l,Some e,_) -> fun acc -> (l,e)::acc) el [] in
     let lel = ("",e) :: lel in          (* Add the operator *)
      texp_apply (texp_ident "Trx.build_apply")
        [texp_loc exp.exp_loc;
         texp_array (List.map (fun (l,e) ->
           texp_tuple [texp_string l;trx_bracket n e]) lel)]

  (* Pretty much like a function *)
  (* rcl: regular cases; ecl: exceptional cases *)
  | Texp_match (e,rcl,ecl,_) ->
      let cl = rcl @ ecl in     (* handle all cases uniformly *)
      let (pl,names,binding_pat) =
        trx_cl cl (wrap_ty_in_code n (Btype.newgenvar ())) in
      texp_apply (texp_ident "Trx.build_match")
        [texp_loc exp.exp_loc;
         texp_pats_names pl names;
         trx_bracket n e;
         texp_int @@ List.length rcl;
         trx_case_list_body n binding_pat exp cl
       ]

  | Texp_try (e,cl) ->                 (* same as Texp_match *)
      let (pl,names,binding_pat) =
        trx_cl cl (wrap_ty_in_code n (Btype.newgenvar ())) in
      texp_apply (texp_ident "Trx.build_try")
        [texp_loc exp.exp_loc;
         texp_pats_names pl names;
         trx_bracket n e;
         trx_case_list_body n binding_pat exp cl
       ]

  | Texp_tuple el ->
      texp_apply (texp_ident "Trx.build_tuple")
        [texp_loc exp.exp_loc;
	 texp_array (List.map (trx_bracket n) el)]

  | Texp_construct (li, cdesc, args) ->
      let lid = qualify_ctor li cdesc in
      texp_apply (texp_ident "Trx.build_construct")
        [texp_loc exp.exp_loc;
         texp_lid lid;
	 texp_array (List.map (trx_bracket n) args)]

  | Texp_variant (l,eo) ->              (* polymorphic variant *)
      texp_apply (texp_ident "Trx.build_variant")
        [texp_loc exp.exp_loc;
         texp_string l;
	 texp_option (map_option (trx_bracket n) eo)]

  | Texp_record (lel,eo) ->
      texp_apply (texp_ident "Trx.build_record")
        [texp_loc exp.exp_loc;
         texp_array (List.map (fun (li,ldesc,e) ->
           texp_tuple [texp_lid (qualify_label li ldesc);
                       trx_bracket n e]) lel);
         texp_option (map_option (trx_bracket n) eo)]

  | Texp_field (e,li,ldesc) ->
      texp_apply (texp_ident "Trx.build_field")
        [texp_loc exp.exp_loc;
         trx_bracket n e;
         texp_lid (qualify_label li ldesc)]

  | Texp_setfield (e1,li,ldesc,e2) ->
      texp_apply (texp_ident "Trx.build_setfield")
        [texp_loc exp.exp_loc;
         trx_bracket n e1;
         texp_lid (qualify_label li ldesc);
         trx_bracket n e2]

  | Texp_array el ->
      texp_apply (texp_ident "Trx.build_array")
        [texp_loc exp.exp_loc;
	 texp_array (List.map (trx_bracket n) el)]

  | Texp_ifthenelse (e,et,efo) ->
      texp_apply (texp_ident "Trx.build_ifthenelse")
        [texp_loc exp.exp_loc;
         trx_bracket n e;
         trx_bracket n et;
	 texp_option (map_option (trx_bracket n) efo)]

  | Texp_sequence (e1,e2) ->
      texp_apply (texp_ident "Trx.build_sequence")
        [texp_loc exp.exp_loc;
	 trx_bracket n e1; trx_bracket n e2]
  | Texp_while (e1,e2) ->
      texp_apply (texp_ident "Trx.build_while")
        [texp_loc exp.exp_loc;
	 trx_bracket n e1; trx_bracket n e2]

  | Texp_for (id, pat, elo, ehi, dir, ebody) ->
      let name =
        begin
          match pat.ppat_desc with
          | Ppat_any -> mknoloc "_for"  (* the typechecker also makes a dummy*)
          | Ppat_var x -> x
          | _  -> assert false
        end
      in
      texp_apply (texp_ident "Trx.build_for")
        [texp_loc exp.exp_loc;
         texp_string_loc name;
         trx_bracket n elo;
         trx_bracket n ehi;
         texp_bool (dir = Upto);
         let var_typ = wrap_ty_in_code n (Ctype.instance_def Predef.type_int) in
         let pat = {pat_loc = exp.exp_loc; pat_extra = [];
                    pat_attributes = [];
                    pat_type = var_typ; pat_env = exp.exp_env;
                    pat_desc = Tpat_var (id,name)} in
         { exp with
           exp_desc =
             Texp_function ("",[texp_case pat (trx_bracket n ebody)],Total);
           exp_type =
            {exp.exp_type with desc =
               Tarrow ("",var_typ, wrap_ty_in_code n ebody.exp_type, Cok)}
         }
       ]

  | Texp_send (e,m,_) ->
      (* We don't check the persistence of the method: after all,
         a method name is somewhat like a polymorphic variant.
         It's perfectly OK to have a function fun x -> x # foo
      *)
      texp_apply (texp_ident "Trx.build_send")
        [texp_loc exp.exp_loc;
	 trx_bracket n e;
         texp_string (match m with
                        | Tmeth_name name -> name
                        | Tmeth_val id -> Ident.name id)]

  | Texp_new (p,li,_) ->
      check_path_quotable "Class" p;
      texp_code ~node_id:"*new*" exp.exp_loc
        (Pexp_new (Location.mkloc (path_to_lid p) li.loc))

  | Texp_instvar (p1,p2,s) ->
      not_supported exp.exp_loc "Objects (Texp_instvar)"
        (* Alternatively: since instance variables are always bound
           at level 0 (for now)
           so this is like a csp variable
        call_trx_mkcsp exp None (path_to_lid p2)
        *)
  | Texp_setinstvar _ -> not_supported exp.exp_loc "Objects (Texp_setinstvar)"
  | Texp_override  _  -> not_supported exp.exp_loc "Objects (Texp_override)"
  | Texp_letmodule (id,s,me,e) -> not_supported exp.exp_loc "let module"

  | Texp_assert e ->
      texp_apply (texp_ident "Trx.build_assert")
        [texp_loc exp.exp_loc; trx_bracket n e]

  | Texp_lazy e ->
      texp_apply (texp_ident "Trx.build_lazy")
        [texp_loc exp.exp_loc; trx_bracket n e]

  | Texp_object (cl,fl) -> not_supported exp.exp_loc "Objects"
  | Texp_pack _         -> not_supported exp.exp_loc "First-class modules"
  (* | _ -> not_supported exp.exp_loc "not yet supported" *)
  | Texp_quote body -> begin
      match body.exp_desc with
      | Texp_sequence(_, exp) ->
          texp_apply (texp_ident "Trx.build_quote")
                     [texp_loc exp.exp_loc; trx_bracket (n+1) exp]
      | _ -> assert false   (* corrupted representation of bracket *)
    end
  | Texp_escape body -> begin
      match body.exp_desc with
      | Texp_sequence(_,exp) ->
          if n = 1 then exp.exp_desc	               (* switch to 0 level *)
          else
            texp_apply (texp_ident "Trx.build_escape")
                       [texp_loc exp.exp_loc; trx_bracket (n-1) exp]
      | _ -> assert false   (* corrupted representation of escape *)
    end
(*TODO:  | CSP(_,li,attrs) -> (* For CSP, we only need to propagate the CSP attr *)
     {exp with
         exp_type = wrap_ty_in_code n exp.exp_type;
         exp_attributes = attrs;
         exp_desc = texp_apply
                     (texp_ident "Trx.dyn_quote") [exp; texp_lid li]}*)


and trx_extra (extra, loc, attr) exp =  (* TODO: take care of attr *)
  let desc =
    match extra with
      (* Should check that cty1 and cty2 contain only globally declared
         type components
       *)
    | Texp_constraint cty ->
        not_supported loc "Texp_constraint"
    | Texp_coerce (cto,ct) ->
        not_supported loc "Texp_coerce"
    | Texp_open (ovf, path, lid, _) ->
       (* I don't need local open since all the constructors
          and identifiers are qualified anyway.
        *)
        exp.exp_desc
          (*
       check_path_quotable "Texp_open" path;
       texp_apply (texp_ident "Trx.build_open")
        [texp_loc exp.exp_loc;
         texp_lid (mkloc (path_to_lid path) lid.loc);
         texp_csp (Obj.repr ovf);
         exp]      (* exp is the result of trx_bracket *)
           *)
    | Texp_poly cto  -> not_supported loc "Texp_poly"
    | Texp_newtype s -> not_supported loc "Texp_newtype"
  in
    {exp with exp_loc = loc; exp_desc = desc} (* type is the same: code *)

and trx_bracket : int -> expression -> expression = fun n exp ->
  let new_desc = trx_bracket_desc n exp in
    List.fold_right
      trx_extra exp.exp_extra
      {exp with exp_type = wrap_ty_in_code n exp.exp_type;
                exp_desc = new_desc}

  (* convert the case list to the function that receives the sequence
     of bound variables and returns the array of translated guards and
     bodies
   *)
and trx_case_list_body : int -> Typedtree.pattern ->
  expression ->  (* used as the template for the result: we use
                    the env, location info *)
  case list -> expression = fun n binding_pat exp cl ->
  (* Translate the future-stage function as the present-stage
     function whose argument is an array of variables
     (should be a tuple, really) and the type
     some_targ code array -> tres code array
     Using array forces a single type to all arguments. Although
     it is phantom anyway, it is still a bummer. Instead of
     array, we should have used a tuple. But then we can't
     generically write build_fun.
   *)
  (* Pattern representing the function's argument:
     array of variables bound by the original pattern, in order.
   *)
  let body =
    texp_array (List.map (fun {c_guard;c_rhs;_} ->
      texp_tuple [texp_option @@ map_option (trx_bracket n) c_guard;
                  trx_bracket n c_rhs])
      cl) in
  { exp with
    exp_desc = Texp_function ("",[texp_case binding_pat body],Total);
    exp_type = {exp.exp_type with desc =
                   Tarrow ("",binding_pat.pat_type, body.exp_type, Cok)}
  }
