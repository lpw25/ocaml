open Parsetree

let rec check_list f xs ys =
  match xs, ys with
  | [], [] -> true
  | x :: xs, y :: ys -> f x y && check_list f xs ys
  | _ :: _, [] -> false
  | [], _ :: _ -> false

let check_option f xo yo =
  match xo, yo with
  | None, None -> true
  | Some x, Some y -> f x y
  | None, Some _ -> false
  | Some _, None -> false

let longer = ref 0
let shorter = ref 0
let dot_diff = ref 0
let hidden_diff = ref 0

let hidden name =
  if name <> "" && name.[0] = '_' then true
  else
    try
      for i = 1 to String.length name - 2 do
        if name.[i] = '_' && name.[i + 1] = '_' then
          raise Exit
      done;
      false
    with Exit -> true

let check_longident lid1 lid2 =
  let rec length = function
    | Longident.Lident name ->
        1, if hidden name then 1 else 0
    | Longident.Ldot(parent, name) ->
        let length, hiddens = length parent in
        length + 1, if hidden name then hiddens + 1 else hiddens
    | Longident.Lapply(func, arg) ->
        let length_func, hiddens_func = length func in
        let length_arg, hiddens_arg = length arg in
        length_func + length_arg, hiddens_func + hiddens_arg
  in
  let length1, hiddens1 = length lid1 in
  let length2, hiddens2 = length lid2 in
  if hiddens1 = hiddens2 then begin
    if length1 < length2 then begin
      dot_diff := !dot_diff + (length2 - length1);
      incr longer
    end else if length1 > length2 then begin
      dot_diff := !dot_diff + (length2 - length1);
      incr shorter
    end
  end else if hiddens1 < hiddens2 then begin
    hidden_diff := !hidden_diff + (hiddens2 - hiddens1);
    incr longer
  end else begin
    hidden_diff := !hidden_diff + (hiddens2 - hiddens1);
    incr shorter
  end;
  true

let check_loc f loc1 loc2 =
  f loc1.Location.txt loc2.Location.txt

let rec check_row_field row1 row2 =
  match row1, row2 with
  | Rtag (_, _, _, cty1), Rtag (_, _, _, cty2) ->
    check_list check_core_type cty1 cty2
  | Rinherit cty1, Rinherit cty2 -> check_core_type cty1 cty2
  | _, _ -> false

and check_package_type (lids1, ctyps1) (lids2, ctyps2) =
  check_loc check_longident lids1 lids2
  && check_list
       (fun (lid1, ctyp1) (lid2, ctyp2) ->
          check_loc check_longident lid1 lid2
          && check_core_type ctyp1 ctyp2)
       ctyps1 ctyps2

and check_core_type_desc ctyp1 ctyp2 =
  match ctyp1, ctyp2 with
  | Ptyp_any, Ptyp_any -> true
  | Ptyp_var _, Ptyp_var _ -> true
  | Ptyp_arrow (_, a1, b1), Ptyp_arrow (_, a2, b2) ->
      check_core_type a1 a2
      && check_core_type b1 b2
  | Ptyp_tuple ctyps1, Ptyp_tuple ctyps2 ->
      check_list check_core_type ctyps1 ctyps2
  | Ptyp_constr (lid1, ctyps1), Ptyp_constr (lid2, ctyps2) ->
      check_loc check_longident lid1 lid2
      && check_list check_core_type ctyps1 ctyps2
  | Ptyp_object (l1, _), Ptyp_object (l2, _) ->
      check_list
        (fun (_, _, ctyp1) (_, _, ctyp2) -> check_core_type ctyp1 ctyp2)
        l1 l2
  | Ptyp_class (lid1, ctyps1), Ptyp_class (lid2, ctyps2) ->
      check_loc check_longident lid1 lid2
      && check_list check_core_type ctyps1 ctyps2
  | Ptyp_alias (ctyp1, _), Ptyp_alias (ctyp2, _) ->
      check_core_type ctyp1 ctyp2
  | Ptyp_variant (rows1, _, _), Ptyp_variant (rows2, _, _) ->
      check_list check_row_field rows1 rows2
  | Ptyp_poly (_, ctyp1), Ptyp_poly (_, ctyp2) ->
      check_core_type ctyp1 ctyp2
  | Ptyp_package pkg1, Ptyp_package pkg2 ->
      check_package_type pkg1 pkg2
  | Ptyp_extension _, Ptyp_extension _ -> true
  | (_, _) -> false

and check_core_type ctyp1 ctyp2 =
  check_core_type_desc ctyp1.ptyp_desc ctyp2.ptyp_desc

let check_type_params params1 params2 =
  check_list
    (fun (cty1, _) (cty2, _) -> check_core_type cty1 cty2)
    params1 params2

let rec check_class_infos f ci1 ci2 =
  check_type_params ci1.pci_params ci2.pci_params
  && f ci1.pci_expr ci2.pci_expr

and check_with_constraint wc1 wc2 =
  match wc1, wc2 with
  | Pwith_type(lid1, decl1), Pwith_type(lid2, decl2) ->
      check_loc check_longident lid1 lid2
      && check_type_declaration decl1 decl2
  | Pwith_module(a1, b1), Pwith_module(a2, b2) ->
      check_loc check_longident a1 a2
      && check_loc check_longident b1 b2
  | Pwith_typesubst decl1, Pwith_typesubst decl2 ->
      check_type_declaration decl1 decl2
  | Pwith_modsubst(_, lid1), Pwith_modsubst(_, lid2) ->
      check_loc check_longident lid1 lid2
  | (_, _) -> false

and check_signature_item_desc desc1 desc2 =
  match desc1, desc2 with
  | Psig_value vd1, Psig_value vd2 ->
      check_value_description vd1 vd2
  | Psig_type (_, decls1), Psig_type (_, decls2) ->
      check_list check_type_declaration decls1 decls2
  | Psig_typext ext1, Psig_typext ext2 ->
      check_type_extension ext1 ext2
  | Psig_exception exn1, Psig_exception exn2 ->
      check_extension_constructor exn1 exn2
  | Psig_module md1, Psig_module md2 ->
      check_module_declaration md1 md2
  | Psig_recmodule mds1, Psig_recmodule mds2 ->
      check_list check_module_declaration mds1 mds2
  | Psig_modtype mtd1, Psig_modtype mtd2 ->
      check_module_type_declaration mtd1 mtd2
  | Psig_open opn1, Psig_open opn2 ->
      check_open_description opn1 opn2
  | Psig_include incl1, Psig_include incl2 ->
      check_include_description incl1 incl2
  | Psig_class cds1, Psig_class cds2 ->
      check_list check_class_description cds1 cds2
  | Psig_class_type ctds1, Psig_class_type ctds2 ->
      check_list check_class_type_declaration ctds1 ctds2
  | Psig_attribute _, Psig_attribute _ -> true
  | Psig_extension _, Psig_extension _ -> true
  | (_, _) -> false

and check_signature_item item1 item2 =
  check_signature_item_desc item1.psig_desc item2.psig_desc

and check_signature sg1 sg2 =
  check_list check_signature_item sg1 sg2

and check_module_type_desc desc1 desc2 =
  match desc1, desc2 with
  | Pmty_ident lid1, Pmty_ident lid2 ->
      check_loc check_longident lid1 lid2
  | Pmty_signature sg1, Pmty_signature sg2 ->
      check_signature sg1 sg2
  | Pmty_functor (_, mtyo1, mty1), Pmty_functor (_, mtyo2, mty2) ->
      check_option check_module_type mtyo1 mtyo2
      && check_module_type mty1 mty2
  | Pmty_with (mty1, wc1), Pmty_with (mty2, wc2) ->
      check_module_type mty1 mty2
      && check_list check_with_constraint wc1 wc2
  | Pmty_alias lid1, Pmty_alias lid2 ->
      check_loc check_longident lid1 lid2
  | Pmty_typeof _, Pmty_typeof _ -> true
  | Pmty_extension _, Pmty_extension _ -> true
  | (_, _) -> false

and check_module_type mty1 mty2 =
  check_module_type_desc mty1.pmty_desc mty2.pmty_desc

and check_module_declaration md1 md2 =
  check_module_type md1.pmd_type md2.pmd_type

and check_module_type_declaration mtd1 mtd2 =
  check_option check_module_type mtd1.pmtd_type mtd2.pmtd_type

and check_open_description opn1 opn2 =
  check_loc check_longident opn1.popen_lid opn2.popen_lid

and check_include_description incl1 incl2 =
  check_module_type incl1.pincl_mod incl2.pincl_mod

and check_class_type_declaration ctd1 ctd2 =
  check_class_infos check_class_type ctd1 ctd2

and check_class_description cd1 cd2 =
  check_class_infos check_class_type cd1 cd2

and check_class_type_field_desc desc1 desc2 =
  match desc1, desc2 with
  | Pctf_inherit clty1, Pctf_inherit clty2 ->
      check_class_type clty1 clty2
  | Pctf_val (_, _, _, cty1), Pctf_val (_, _, _, cty2) ->
      check_core_type cty1 cty2
  | Pctf_method (_, _, _, cty1), Pctf_method (_, _, _, cty2) ->
      check_core_type cty1 cty2
  | Pctf_constraint (a1, b1), Pctf_constraint (a2, b2) ->
      check_core_type a1 a2
      && check_core_type b1 b2
  | Pctf_attribute _, Pctf_attribute _ -> true
  | Pctf_extension _, Pctf_extension _ -> true
  | (_, _) -> false

and check_class_type_field ctf1 ctf2 =
  check_class_type_field_desc ctf1.pctf_desc ctf2.pctf_desc

and check_class_signature csig1 csig2 =
  check_core_type csig1.pcsig_self csig2.pcsig_self
  && check_list check_class_type_field csig1.pcsig_fields csig2.pcsig_fields

and check_class_type_desc desc1 desc2 =
  match desc1, desc2 with
  | Pcty_constr (lid1, ctys1), Pcty_constr (lid2, ctys2) ->
      check_loc check_longident lid1 lid2
      && check_list check_core_type ctys1 ctys2
  | Pcty_signature csig1, Pcty_signature csig2 ->
      check_class_signature csig1 csig2
  | Pcty_arrow (_, cty1, clty1), Pcty_arrow (_, cty2, clty2) ->
      check_core_type cty1 cty2
      && check_class_type clty1 clty2
  | Pcty_extension _, Pcty_extension _ -> true
  | (_, _) -> false

and check_class_type clty1 clty2 =
  check_class_type_desc clty1.pcty_desc clty2.pcty_desc

and check_label_declaration ld1 ld2 =
  check_core_type ld1.pld_type ld2.pld_type

and check_constructor_arguments args1 args2 =
  match args1, args2 with
  | Pcstr_tuple ctys1, Pcstr_tuple ctys2 ->
      check_list check_core_type ctys1 ctys2
  | Pcstr_record lds1, Pcstr_record lds2 ->
      check_list check_label_declaration lds1 lds2
  | _, _ -> false

and check_constructor_declaration cd1 cd2 =
  check_constructor_arguments cd1.pcd_args cd2.pcd_args
  && check_option check_core_type cd1.pcd_res cd2.pcd_res

and check_type_kind kind1 kind2 =
  match kind1, kind2 with
  | Ptype_abstract, Ptype_abstract -> true
  | Ptype_variant cds1, Ptype_variant cds2 ->
      check_list check_constructor_declaration cds1 cds2
  | Ptype_record lds1, Ptype_record lds2 ->
      check_list check_label_declaration lds1 lds2
  | Ptype_open, Ptype_open -> true
  | (_, _) -> false

and check_type_declaration decl1 decl2 =
  check_type_params decl1.ptype_params decl2.ptype_params
  && check_list
       (fun (a1, b1, _) (a2, b2, _) ->
          check_core_type a1 a2
          && check_core_type b1 b2)
       decl1.ptype_cstrs decl2.ptype_cstrs
  && check_type_kind decl1.ptype_kind decl2.ptype_kind
  && check_option check_core_type decl1.ptype_manifest decl2.ptype_manifest

and check_extension_constructor_kind kind1 kind2 =
  match kind1, kind2 with
  | Pext_decl(args1, cty1), Pext_decl(args2, cty2) ->
      check_constructor_arguments args1 args2
      && check_option check_core_type cty1 cty2
  | Pext_rebind lid1, Pext_rebind lid2 ->
      check_loc check_longident lid1 lid2
  | _, _ -> false

and check_extension_constructor ext1 ext2 =
  check_extension_constructor_kind ext1.pext_kind ext2.pext_kind

and check_type_extension ext1 ext2 =
  check_loc check_longident ext1.ptyext_path ext2.ptyext_path
  && check_type_params ext1.ptyext_params ext2.ptyext_params
  && check_list check_extension_constructor
       ext1.ptyext_constructors ext2.ptyext_constructors

and check_value_description vd1 vd2 =
  check_core_type vd1.pval_type vd2.pval_type

let parse sourcefile =
  Location.input_name := sourcefile;
  let ic = open_in_bin sourcefile in
  try
    let lexbuf = Lexing.from_channel ic in
    Location.init lexbuf sourcefile;
    let ast = Parse.interface lexbuf in
    close_in ic;
    ast
  with x -> close_in ic; raise x

let usage =
  "checkpaths FILE1.mli FILE2.mli"

let () =
  let args = ref [] in
  Arg.parse []
    (fun arg -> args := arg :: !args)
    usage;
  let file1, file2 =
    match !args with
    | [file2; file1] -> file1, file2
    | _ ->
      Arg.usage [] usage;
      exit 1
  in
  let ast1 = parse file1 in
  let ast2 = parse file2 in
  if not (check_signature ast1 ast2) then begin
    Printf.eprintf "Signatures not equal\n%!";
    exit 1
  end;
  Printf.printf "Longer: %i\nShorter: %i\nDots: %i\nUnderscores: %i\n%!"
    !longer !shorter !dot_diff !hidden_diff
