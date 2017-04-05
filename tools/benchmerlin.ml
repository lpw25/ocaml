
let char_size = 8
let int_chars = (Sys.int_size + (char_size - 1)) / char_size

let int_array_of_string str =
  let length = String.length str in
  let size = (length + (int_chars - 1)) / int_chars in
  let array = Array.make size 0 in
  for i = 0 to size - 1 do
    let int = ref 0 in
    for j = 0 to int_chars - 1 do
      let index = i * int_chars + j in
      if index < length then begin
        let code = Char.code str.[index] in
        let shift = j * char_size in
        int := !int lor (code lsl shift);
      end;
    done;
    array.(i) <- !int;
  done;
  array

let request state length =
  Random.State.int state length = 0

let stop state target length =
  Random.State.int state (length * (target - 1)) = 0

let random_step state jump_freq length last =
  if Random.State.int state (length * jump_freq) = 0 then begin
    Random.State.int state length
  end else if Random.State.bool state then begin
    let next = last + 1 in
    if next <= length then next
    else length
  end else begin
    let next = last - 1 in
    if next >= 0 then next
    else 0
  end

let generate seed locations jumps target =
  let state = Random.State.make seed in
  let length = Array.length locations in
  let jump_freq = (target - 1) / jumps in
  let first = Random.State.int state length in
  let rec loop last acc =
    if stop state target length then acc
    else begin
      let next = random_step state jump_freq length last in
      let acc =
        if request state length then locations.(next) :: acc
        else acc
      in
      loop next acc
    end
  in
  loop first [locations.(first)]

let print_loc ppf loc =
  let open Location in
  let open Lexing in
  let pos = loc.loc_start in
  let line = pos.pos_lnum in
  let col = pos.pos_cnum - pos.pos_bol in
  Format.fprintf ppf "%i:%i" line col

let enumerate ast =
  let open Ast_iterator in
  let expressions = ref [] in
  let expr it exp =
    let loc = exp.Parsetree.pexp_loc in
    expressions := loc :: !expressions;
    default_iterator.expr it exp
  in
  let enumerator = { default_iterator with expr } in
  enumerator.structure enumerator ast;
  Array.of_list !expressions

let parse_impl sourcefile =
  Location.input_name := sourcefile;
  let ic = open_in_bin sourcefile in
  try
    let lexbuf = Lexing.from_channel ic in
    Location.init lexbuf sourcefile;
    let ast = Parse.implementation lexbuf in
    close_in ic;
    ast
  with x -> close_in ic; raise x

let usage =
  "benchmerlin MERLIN FILE"

let () =
  let args = ref [] in
  Arg.parse []
    (fun arg -> args := arg :: !args)
    usage;
  let _merlin, file =
    match !args with
    | [file; merlin] -> merlin, file
    | _ ->
      Arg.usage [] usage;
      exit 1
  in
  let jumps = 1 in
  let target = 10 in
  let ast = parse_impl file in
  let locations = enumerate ast in
  let seed = int_array_of_string file in
  let session = generate seed locations jumps target in
  Format.printf "%a\n%!"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space print_loc)
    session
