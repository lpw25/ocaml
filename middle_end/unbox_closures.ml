(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

module A = Simple_value_approx
module E = Inline_and_simplify_aux.Env

let rewrite_function_decl
    ~closure_id
    ~(function_decls : Flambda.function_declarations)
    ~(function_decl : Flambda.function_declaration)
    ~(free_vars : Variable.t Variable.Map.t)
    ~additional_specialised_args =
  (* Format.printf "rewrite %a@." Closure_id.print closure_id; *)
  let params_for_vars_bound_by_closure =
    Variable.Map.of_set
      (fun var -> Variable.rename ~append:"_unboxed_bound" var)
      (* (ignore function_decls; ignore closure_id; Variable.Map.keys free_vars) *)
      (Flambda_utils.variables_bound_by_the_closure closure_id function_decls)
  in
  let closure_element_params =
    List.map snd (Variable.Map.bindings params_for_vars_bound_by_closure)
  in
  let all_params = function_decl.params @ closure_element_params in
  let body_using_params_not_closures =
    Flambda_utils.toplevel_substitution
      params_for_vars_bound_by_closure
      function_decl.body
    |> Flambda_iterators.map_project_var_to_expr_opt
      ~f:(fun { var } ->
          let var = Var_within_closure.unwrap var in
          match Variable.Map.find var params_for_vars_bound_by_closure with
          | exception Not_found ->
            (* Format.printf "not found %a@." Variable.print var; *)
            None
          | var' ->
            (* Format.printf "found %a -> %a@." Variable.print var Variable.print var'; *)
            Some (Flambda.Var var'))
  in
  let additional_specialised_args =
    Variable.Map.fold
      (fun var_bound_by_closure new_param
        additional_specialised_args ->
        let specialised_to =
          Variable.Map.find var_bound_by_closure free_vars
        in
        Variable.Map.add new_param specialised_to additional_specialised_args)
      params_for_vars_bound_by_closure
      additional_specialised_args
  in
  additional_specialised_args,
  Flambda.create_function_declaration
    ~params:all_params
    ~body:body_using_params_not_closures
    ~stub:function_decl.stub
    ~dbg:function_decl.dbg
    ~inline:function_decl.inline

let create_wrapper
    ~(function_decls : Flambda.function_declarations)
    ~(function_decl : Flambda.function_declaration)
    ~fun_var =
  let closure_id = Closure_id.wrap fun_var in
  let new_fun_var = Variable.rename ~append:"_unbox_closures" fun_var in
  let wrapper_params =
    List.map (fun param -> Variable.rename ~append:"_wrapper_param" param)
      function_decl.params
  in
  let extra_args =
    Variable.Set.elements
      (Flambda_utils.variables_bound_by_the_closure closure_id function_decls)
  in
  let wrapper_body : Flambda.t =
    Apply {
      func = new_fun_var;
      args = wrapper_params @ extra_args;
      kind = Direct (Closure_id.wrap new_fun_var);
      dbg = Debuginfo.none;
      inline = Default_inline;
    }
  in
  new_fun_var,
  Flambda.create_function_declaration
    ~params:wrapper_params
    ~body:wrapper_body
    ~stub:true
    ~dbg:Debuginfo.none
    ~inline:Default_inline

let add_wrapper
    ~fun_var
    ~(function_decls : Flambda.function_declarations)
    ~(function_decl : Flambda.function_declaration)
    ~(free_vars : Variable.t Variable.Map.t)
    ~funs
    ~additional_specialised_args =
  let closure_id = Closure_id.wrap fun_var in
  if Variable.Set.is_empty
      (Flambda_utils.variables_bound_by_the_closure closure_id function_decls)
     || function_decl.stub
  then
    let funs = Variable.Map.add fun_var function_decl funs in
    funs, additional_specialised_args
  else
    let additional_specialised_args, rewritten_function_decl =
      rewrite_function_decl ~closure_id ~free_vars ~function_decls ~function_decl
        ~additional_specialised_args
    in
    let new_fun_var, wrapper =
      create_wrapper ~fun_var ~function_decls ~function_decl
    in
    let funs =
      Variable.Map.add new_fun_var rewritten_function_decl
        (Variable.Map.add fun_var wrapper funs)
    in
    funs, additional_specialised_args

let do_rewrite_set_of_closures (set_of_closures : Flambda.set_of_closures) =
  let function_decls = set_of_closures.function_decls in
  let free_vars = set_of_closures.free_vars in
  let funs, additional_specialised_args =
    Variable.Map.fold
      (fun fun_var function_decl (funs, additional_specialised_args) ->
         add_wrapper ~function_decls ~fun_var ~free_vars
           ~function_decl
           ~funs ~additional_specialised_args)
      function_decls.funs
      (Variable.Map.empty, Variable.Map.empty)
  in
  let specialised_args =
    try
      Variable.Map.disjoint_union set_of_closures.specialised_args
        additional_specialised_args
        ~eq:Variable.equal
    with _ ->
      assert false
  in
  let function_decls =
    Flambda.update_function_declarations function_decls ~funs
  in
  Flambda.create_set_of_closures
    ~function_decls
    ~free_vars:set_of_closures.free_vars
    ~specialised_args

let contains_stub (fun_decls : Flambda.function_declarations) =
  let number_of_stub_functions =
    Variable.Map.cardinal
      (Variable.Map.filter (fun _ { Flambda.stub } -> stub)
         fun_decls.funs)
  in
  number_of_stub_functions > 0

let introduce_specialised_args_for_free_vars
    (set_of_closures : Flambda.set_of_closures) =
  let candidate_for_transformation =
    not (Variable.Map.is_empty set_of_closures.free_vars)
    && (Variable.Map.is_empty set_of_closures.specialised_args
        || contains_stub set_of_closures.function_decls)
  in

  if not candidate_for_transformation then
    set_of_closures
  else
    (* let () = *)
    (*   Format.eprintf "Before Unbox_closures:@ %a@.@." *)
    (*     Flambda.print_set_of_closures set_of_closures *)
    (* in *)
    let set_of_closures = do_rewrite_set_of_closures set_of_closures in
    (* let () = *)
    (*   Format.eprintf "After Unbox_closures:@ %a@.@." *)
    (*     Flambda.print_set_of_closures set_of_closures *)
    (* in *)
    set_of_closures

let replace_free_vars_by_equal_specialised_args
    ~(function_decl : Flambda.function_declaration)
    ~back_free_vars
    ~specialised_args =
  let params_for_equal_free_vars =
    List.fold_left (fun subst param ->
        match Variable.Map.find param specialised_args with
        | exception Not_found ->
          (* param is not specialised *)
          subst
        | outside_var ->
          match Variable.Map.find outside_var back_free_vars with
          | exception Not_found ->
            (* No free variables equal to the param *)
            subst
          | set ->
            (* Replace the free variables equal to an parameter *)
            Variable.Set.fold (fun free_var subst ->
                Variable.Map.add free_var param subst)
              set subst)
      Variable.Map.empty function_decl.params
  in
  if Variable.Map.is_empty params_for_equal_free_vars then
    function_decl
  else
    let body =
      Flambda_utils.toplevel_substitution
        params_for_equal_free_vars
        function_decl.body
    in
    Flambda.create_function_declaration
      ~params:function_decl.params
      ~body:body
      ~stub:function_decl.stub
      ~dbg:function_decl.dbg
      ~inline:function_decl.inline

let rewrite_function_declaration
    ~free_vars
    ~function_decl
    ~specialised_args =
  let back_free_vars =
    Variable.Map.fold (fun var outside_var map ->
        let set =
          match Variable.Map.find outside_var map with
          | exception Not_found -> Variable.Set.singleton var
          | set -> Variable.Set.add var set
        in
        Variable.Map.add outside_var set map)
      free_vars Variable.Map.empty
  in
    replace_free_vars_by_equal_specialised_args
      ~function_decl
      ~back_free_vars
      ~specialised_args

let replace_free_vars_by_equal_specialised_args
    (set_of_closures : Flambda.set_of_closures) =
  let back_free_vars =
    Variable.Map.fold (fun var outside_var map ->
        let set =
          match Variable.Map.find outside_var map with
          | exception Not_found -> Variable.Set.singleton var
          | set -> Variable.Set.add var set
        in
        Variable.Map.add outside_var set map)
      set_of_closures.free_vars Variable.Map.empty
  in
  let funs =
    Variable.Map.map
      (fun function_decl ->
         replace_free_vars_by_equal_specialised_args
           ~function_decl
           ~back_free_vars
           ~specialised_args:set_of_closures.free_vars)
      set_of_closures.function_decls.funs
  in
  let function_decls =
    Flambda.update_function_declarations
      set_of_closures.function_decls ~funs
  in
  Flambda.create_set_of_closures
    ~function_decls
    ~free_vars:set_of_closures.free_vars
    ~specialised_args:set_of_closures.specialised_args
