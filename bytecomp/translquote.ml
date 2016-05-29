open Misc
open Asttypes
open Types
open Typedtree
open Lambda

let camlinternalQuote =
  lazy
    (match Env.open_pers_signature
             "CamlinternalQuote" Env.initial_safe_string with
     | exception Not_found ->
         fatal_error "Module CamlinternalQuote unavailable."
     | env -> ident, env)

let combinator modname field =
  lazy
    (let (ident, env) = Lazy.force camlinternalQuote in
     let lid = Longident.Ldot(Longident.Lident modname, field) in
     match Env.lookup_value lid env with
     | (Path.Pdot(Path.Pdot(Pident ident, _, pos1), _, pos2), _) ->
         Lprim(Pfield pos2,
               [Lprim(Pfield pos1,
                     [Lprim(Pgetglobal ident, [])])])
     | _ ->
         fatal_error
           "Primitive CamlinternalQuote."^modname^"."^field^" not found."
     | exception Not_found ->
        fatal_error
          "Primitive CamlinternalQuote."^modname^"."^field^" not found.")

module Loc = struct
  let none = combinator "Loc" "none"
  let unmarshal = combinator "Loc" "unmarshal"
end

module Name = struct
  let mk = combinator "Name" "mk"
  let unmarshal = combinator "Name" "unmarshal"
end

module Var : sig
  let name = combinator "Var" "name"
end

module Constant : sig
  let unmarshal = combinator "Constant" "unmarshal"
end

module Ident : sig
  let unmarshal = combinator "Ident" "unmarshal"
end

module Label : sig
  let none = combinator "Label" "none"
  let of_string = combinator "Label" "of_string"
end

module Variant : sig
  let of_string = combinator "Variant" "of_string"
end

module Method : sig
  let of_string = combinator "Method" "of_string"
end

module Pat : sig
  let any = combinator "Pat" "any"
  let var = combinator "Pat" "var"
  let alias = combinator "Pat" "alias"
  let constant = combinator "Pat" "constant"
  let interval = combinator "Pat" "interval"
  let tuple = combinator "Pat" "tuple"
  let construct = combinator "Pat" "construct"
  let variant = combinator "Pat" "variant"
  let record = combinator "Pat" "record"
  let array = combinator "Pat" "array"
  let or_ = combinator "Pat" "or"
  let type_ = combinator "Pat" "type"
  let lazy_ = combinator "Pat" "lazy"
  let exception_ = combinator "Pat" "exception"
end

module rec Case : sig
  let nonbinding = combinator "Case" "nonbinding"
  let binding = combinator "Case" "binding"
end

and Exp : sig
  let var = combinator "Exp" "var"
  let ident = combinator "Exp" "ident"
  let constant = combinator "Exp" "constant"
  let let_simple = combinator "Exp" "let_simple"
  let let_rec_simple = combinator "Exp" "let_rec_simple"
  let let_ = combinator "Exp" "let_"
  let fun_nonbinding = combinator "Exp" "fun_nonbinding"
  let fun_simple = combinator "Exp" "fun_simple"
  let fun_ = combinator "Exp" "fun_"
  let function_ = combinator "Exp" "function_"
  let apply = combinator "Exp" "apply"
  let match_ = combinator "Exp" "match_"
  let try_ = combinator "Exp" "try_"
  let tuple = combinator "Exp" "tuple"
  let construct = combinator "Exp" "construct"
  let variant = combinator "Exp" "variant"
  let record = combinator "Exp" "record"
  let field = combinator "Exp" "field"
  let setfield = combinator "Exp" "setfield"
  let array = combinator "Exp" "array"
  let ifthenelse = combinator "Exp" "ifthenelse"
  let sequence = combinator "Exp" "sequence"
  let while_ = combinator "Exp" "while_"
  let for_nonbinding = combinator "Exp" "for_nonbinding"
  let for_simple = combinator "Exp" "for_simple"
  let send = combinator "Exp" "send"
  let assert_ = combinator "Exp" "assert_"
  let lazy_ = combinator "Exp" "lazy_"
  let open_ = combinator "Exp" "open_"
  let quote = combinator "Exp" "quote"
  let escape = combinator "Exp" "escape"
end

let use comb =
  Lazy.force comb

let apply loc comb args =
  let comb = Lazy.force comb in
    Lapply (comb, args, loc)

let string s =
  Lconst (Const_base (Const_string(s, None)))

let marshal x =
  let s = Marshal.to_string x in
    string s

let true_ = Lconst(Const_pointer 1)

let false_ = Lconst(Const_pointer 0)

let none =  Lconst(Const_pointer 0)

let some x = Lprim(Pmakeblock(0, Immutable), [x])

let option opt =
  match opt with
  | None -> none
  | Some x -> some x

let nil = Lconst(Const_pointer 0)

let cons hd tl = Lprim(Pmakeblock(0, Immutable), [hd; tl])

let rec list list =
  match list with
  | [] -> nil
  | hd :: tl -> cons hd (list tl)

let pair (x, y) =
  Lprim(Pmakeblock(0, Immutable), [x; y])

let func ids body =
  Lfunction(Curried, ids, body)

let bind id def body =
  Llet(Strict, id, def, body)

let quote_loc loc =
  if loc = none then use Loc.none
  else apply Location.none Loc.unmarshal [marshal loc]

let quote_constant loc const =
  apply loc Const.unmarshal [marshal const]

let quote_variant loc variant =
  apply loc Variant.of_string [string variant]

let quote_method loc meth =
  let name =
    match meth with
    | Tmeth_name name -> name
    | Tmeth_val id -> Ident.name id
  in
  apply loc Method.of_string [string name]

let lid_of_path p =
  let rec loop = function
    | Pident id ->
      if Ident.global id then Lid (Ident.name id)
      else raise Exit
    | Pdot(p, s, _) -> Ldot(loop p, s)
    | Papply _ -> raise Exit
  in
    match loop p with
    | lid -> Some lid
    | exception Exit -> None

let lid_of_type_path env ty =
  let desc =
    (Ctype.repr (Ctype.expand_head_opt env (Ctype.correct_levels ty))).desc
  in
  match desc with
  | Tconstr(p, _, _) -> lid_of_path p
  | _ -> None

let quote_constructor env loc constr =
  let lid =
    match lid_of_type_path env constr.cstr_res with
    | None -> fatal_error "No global path for variant constructor"
    | Some (Lident _) -> Lident constr.cstr_name
    | Some (Ldot(lid, _)) -> Ldot(lid, constr.cstr_name)
  in
  apply loc Ident.unmarshall [marshall lid]

let quote_label env loc lbl =
  let lid =
    match lid_of_type_path env lbl.lbl_res with
    | None -> fatal_error "No global path for record label"
    | Some (Lident _) -> Lident constr.lbl_name
    | Some (Ldot(lid, _)) -> Ldot(lid, constr.lbl_name)
  in
  apply loc Ident.unmarshall [marshall lid]

let rec quote_pattern p =
  let env = p.pat_env in
  let loc = p.pat_loc in
  match p.pat_desc with
  | Tpat_any -> apply loc Pat.any [quote_loc loc]
  | Tpat_var(id, _) -> apply loc Pat.var [quote_loc loc; Lvar id]
  | Tpat_alias(pat, id, _) ->
      let pat = quote_pattern pat in
      apply loc Pat.alias [quote_loc loc; pat; Lvar id]
  | Tpat_constant const ->
      let const = quote_constant loc const in
      apply loc Pat.const [quote_loc loc; const]
  | Tpat_tuple pats ->
      let pats = List.map quote_pattern pats in
      apply loc Pat.tuple [quote_loc loc; list pats]
  | Tpat_construct(lid, constr, args) ->
      let constr = quote_constructor env lid.loc constr in
      let args =
        match args with
        | [] -> None
        | _ :: _ ->
            let args = List.map quote_pattern args in
            Some (apply loc Pat.tuple [quote_loc loc; list args])
      in
      apply loc Pat.construct [quote_loc loc; constr; option args]
  | Tpat_variant(variant, argo, _) ->
      let variant = quote_variant loc variant in
      let argo = Utils.may_map quote_pattern argo in
      apply loc Pat.variant [quote_loc loc; variant; option argo]
  | Tpat_record(lbl_pats, closed) ->
      let lbl_pats =
        List.map
          (fun (lid, lbl, pat) ->
            let lbl = quote_label env lid.loc lbl in
            let pat = quote_pattern pat in
            pair (lbl, pat))
          lbl_pats
      in
      let closed =
        match closed with
        | Asttypes.Closed -> true_
        | Asttypes.Open -> false_
      in
      apply loc Pat.record [quote_loc loc; list lbl_pats; closed]
  | Tpat_array pats ->
      let pats = List.map quote_pattern pats in
      apply loc Pat.array [quote_loc loc; list pats]
  | Tpat_or(pat1, pat2, _) ->
      let pat1 = quote_pattern pat1 in
      let pat2 = quote_pattern pat2 in
      apply loc Pat.or_ [quote_loc loc; pat1; pat2]
  | Tpat_lazy pat ->
      let pat = quote_pattern pat in
      apply loc Pat.lazy_ [quote_loc loc; pat]

let bindings = function
  | Tpat_any -> []
  | Tpat_var(id, name) -> [id, name]
  | Tpat_alias(pat, id, name) -> (id, name) :: bindings pat
  | Tpat_constant _ -> []
  | Tpat_tuple pats -> List.concat (List.map bindings pats)
  | Tpat_construct(_, _, args) -> List.concat (List.map bindings args)
  | Tpat_variant(_, argo, _) -> begin
      match argo with
      | None -> []
      | Some arg -> bindings arg
    end
  | Tpat_record(lbl_pats, _) ->
      List.concat
        (List.map
          (fun (_, _, pat) -> bindings pat)
          lbl_pats)
  | Tpat_array pats -> List.concat (List.map bindings pats)
  | Tpat_or(pat1, pat2, _) ->
      bindings pat1 @ bindings pat2
  | Tpat_lazy pat -> bindings pat

type case =
  | Non_binding of lambda * lambda
  | Simple of string loc * lambda
  | Full of string loc list * lambda

type cases =
  | Single of case
  | Multiple of case list

let rec quote_cases transl stage cases =
  match cases with
  | [case] when case.c_guard = None -> begin
      match case.c_lhs with
      | Tpat_var(id, name) ->
          let body = quote_expression transl stage case.c_rhs in
          Simple(name, func [id] body)
      | pat ->
          match bindings pat with
          | [] ->
              let pat = quote_pattern pat in
              let exp = quote_expression transl stage case.c_rhs in
              Non_binding(pat, exp)
          | id_names ->
              let ids = List.map fst id_names in
              let names = List.map snd id_names in
              let pat = quote_pattern pat in
              let exp = quote_expression transl stage case.c_rhs in
              let pat_id = Ident.create "pattern" in
              let exp_id = Ident.create "expression" in
              let body =
                bind pat_id pat
                  (bind exp_id exp
                    (pair (Lvar pat_id, Lvar exp_id)))
              in
              Single(names, func ids body)
    end
  | cases ->



and quote_expression transl stage e =
  let env = e.exp_env in
  let loc = e.exp_loc in
  match e.exp_desc with
  | Texp_ident(path, _, _) -> begin
      match lid_of_path path with
      | Some lid ->
          let lid = apply loc Ident.unmarshall [marshall lid] in
          apply loc Exp.ident [quote_loc loc; lid]
      | None ->
          match path with
          | Pident id ->
              (* TODO: properly check stage *)
              apply loc Exp.var [quote_loc loc; Lvar id]
          | Pdot _ | Papply _ ->
              fatal_error "No global path for identifier"
    end
  | Texp_constant const ->
      let const = quote_constant loc const in
      apply loc Exp.constant [quote_loc loc; const]
  | Texp_let of rec_flag * value_binding list * expression
  | Texp_function(label, cases, _) -> begin
      match quote_cases cases with
      | 

    end
  | Texp_apply of expression * (label * expression option * optional) list
  | Texp_match of expression * case list * case list * partial
  | Texp_try of expression * case list
  | Texp_tuple exps ->
      let exps = List.map (quote_expression transl stage) exps in
      apply loc Exp.tuple [quote_loc loc; list exps]
  | Texp_construct(lid, constr, args) ->
      let constr = quote_constructor env lid.loc constr in
      let args =
        match args with
        | [] -> None
        | _ :: _ ->
            let args = List.map (quote_expression transl stage) args in
            Some (apply loc Pat.tuple [quote_loc loc; list args])
      in
      apply loc Exp.construct [quote_loc loc; constr; option args]
  | Texp_variant(variant, argo) ->
      let variant = quote_variant loc variant in
      let argo = Utils.may_map (quote_expression transl stage) argo in
      apply loc Exp.variant [quote_loc loc; variant; option argo]
  | Texp_record(lbl_exps, base) ->
      let lbl_exps =
        List.map
          (fun (lid, lbl, exp) ->
            let lbl = quote_label env lid.loc lbl in
            let exp = quote_expression transl stage exp in
            pair (lbl, exp))
          lbl_exps
      in
      let base = Misc.may_map quote_expression base in
      apply loc Exp.record [quote_loc loc; list lbl_exps; option base]
  | Texp_field(rcd, lid, lbl) ->
      let rcd = quote_expression transl stage rcd in
      let lbl = quote_label env lid.loc lbl in
      apply loc Exp.field [quote_loc loc; rcd; lbl]
  | Texp_setfield(rcd, lid, lbl, exp) ->
      let rcd = quote_expression transl stage rcd in
      let lbl = quote_label env lid.loc lbl in
      let exp = quote_expression transl stage exp in
      apply loc Exp.setfield [quote_loc loc; rcd; lbl; exp]
  | Texp_array exps ->
      let exps = List.map (quote_expression transl stage) exps in
      apply loc Exp.array [quote_loc loc; list exps]
  | Texp_ifthenelse(cond, then_, else_) ->
      let cond = quote_expression transl stage cond in
      let then_ = quote_expression transl stage then_ in
      let else_ = Utils.may_map (quote_expression transl stage) else_ in
      apply loc Exp.ifthenelse [quote_loc loc; cond; then_; option else_]
  | Texp_sequence(exp1, exp2) ->
      let exp1 = quote_expression transl stage exp1 in
      let exp2 = quote_expression transl stage exp2 in
      apply loc Exp.sequence [quote_loc loc; exp1; exp2]
  | Texp_while(cond, body) ->
      let cond = quote_expression transl stage cond in
      let body = quote_expression transl stage body in
      apply loc Exp.while_ [quote_loc loc; cond; body]
  | Texp_for of
      Ident.t * Parsetree.pattern * expression * expression * direction_flag *
        expression
  | Texp_send(obj, meth, _) ->
      let obj = quote_expression transl stage obj in
      let meth = quote_method meth in
      apply loc Exp.send [quote_loc loc; obj; meth]
  | Texp_assert exp ->
      let exp = quote_expression transl stage exp in
      apply loc Exp.assert_ [quote_loc loc; exp]
  | Texp_lazy exp ->
      let exp = quote_expression transl stage exp in
      apply loc Exp.lazy_ [quote_loc loc; exp]
  | Texp_quote exp ->
      let exp = quote_expression (stage + 1) exp in
      apply loc Exp.quote_ [quote_loc loc; exp]
  | Texp_escape exp ->
      if stage > 0 then begin
        let exp = quote_expression (stage + 1) exp in
        apply loc Exp.escape_ [quote_loc loc; exp]
      end else transl exp
  | Texp_new _ | Texp_instvar _ | Texp_setinstvar _ | Texp_override _
  | Texp_letmodule _ | Texp_object _ | Texp_pack _ ->
      fatal_error "Expression cannot be quoted"
