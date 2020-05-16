
let rec sep_last =
  function
    [] -> failwith "sep_last"
  | [hd] -> hd, []
  | hd :: tl -> let (l, tl) = sep_last tl in l, hd :: tl

let input_magic ic magic =
  let maglen = String.length magic in
  let b = Bytes.create maglen in really_input ic b 0 maglen; Bytes.to_string b

let input_implem ic =
  if Config.ast_impl_magic_number <>
       input_magic ic Config.ast_impl_magic_number
  then
    failwith "input_implem: bad magic number"
  else let _ = input_value ic in (input_value ic : Parsetree.structure)

let input_interf ic =
  if Config.ast_intf_magic_number <>
       input_magic ic Config.ast_intf_magic_number
  then
    failwith "input_interf: bad magic number"
  else let _ = input_value ic in (input_value ic : Parsetree.signature)

let output_magic oc magic = output_string oc magic

let output_interf fname oc (pt : Parsetree.signature) =
  output_string oc Config.ast_intf_magic_number;
  output_value oc fname;
  output_value oc pt;
  flush oc

let output_implem fname oc (pt : Parsetree.structure) =
  output_string oc Config.ast_impl_magic_number;
  output_value oc fname;
  output_value oc pt;
  flush oc

let parse_interf fname ic =
  let lb = Lexing.from_channel ic in
  Location.init lb fname; Parse.interface lb

let parse_implem fname ic =
  let lb = Lexing.from_channel ic in
  Location.init lb fname; Parse.implementation lb

let print_interf oc v =
  let ofmt = Format.formatter_of_out_channel oc in
  Pprintast.signature ofmt v; Format.pp_print_flush ofmt ()

let print_implem oc v =
  let ofmt = Format.formatter_of_out_channel oc in
  Pprintast.structure ofmt v; Format.pp_print_flush ofmt ()

let binary_input = ref false
let binary_output = ref false
let files = ref []
let filetype = ref None

let set_impl s = filetype := Some "-impl"
let set_intf s = filetype := Some "-intf"

let passthru paf prf ic oc = (ic |> paf) |> prf oc

let papr_official () =
  Arg.
  (parse
     ["-binary-input", Set binary_input, " binary input";
      "-binary-output", Set binary_output, " binary output";
      "-impl", Unit set_impl, " implementation";
      "-intf", Unit set_intf, " interface"]
     (fun s -> files := s :: !files) "papr_official: usage");
  let open_or opener ifminus =
    function
      "-" -> ifminus, ""
    | f -> opener f, f
  in
  let ((ic, ifile), (oc, _)) =
    match List.rev !files with
      [] -> (stdin, ""), (stdout, "")
    | [ifile] -> open_or open_in stdin ifile, (stdout, "")
    | [ifile; ofile] ->
        open_or open_in stdin ifile, open_or open_out stdout ofile
    | _ -> failwith "too many filenames provided"
  in
  begin match !filetype, !binary_input, !binary_output with
    Some "-impl", true, true ->
      failwith "cannot have both binary input and output"
  | Some "-impl", true, false -> passthru input_implem print_implem ic oc
  | Some "-impl", false, true ->
      passthru (parse_implem ifile) (output_implem ifile) ic oc
  | Some "-impl", false, false ->
      passthru (parse_implem ifile) print_implem ic oc
  | Some "-intf", true, true ->
      failwith "cannot have both binary input and output"
  | Some "-intf", true, false -> passthru input_interf print_interf ic oc
  | Some "-intf", false, true ->
      passthru (parse_interf ifile) (output_interf ifile) ic oc
  | Some "-intf", false, false ->
      passthru (parse_interf ifile) print_interf ic oc
  | _ -> failwith "unrecognized filetype"
  end;
  close_out oc;
  close_in ic

let _ = papr_official ()

(*
;;; Local Variables: ***
;;; mode:tuareg ***
;;; End: ***

*)
