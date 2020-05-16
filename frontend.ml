(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

open Findlib;;

exception Usage;;
exception Silent_error;;
  
type mode =
    M_reinstall_if_diff | M_preprocess | M_passthru
;;


type psubst =
    Const of string
  | Percent of string * modifier
  | Lookup of string * modifier

and modifier =
  | Plain
  | Plus
;;

let except e =
 let rec except_e = function
     [] -> []
   | elem::l -> if e = elem then except_e l else elem::except_e l
 in except_e
;;

let sys_error code arg =
  if arg = "" then
    Sys_error (Unix.error_message code)
  else
    Sys_error (arg ^ ": " ^ Unix.error_message code)


let slashify s =
  match Findlib_config.system with
    | "mingw" | "mingw64" | "cygwin" ->
        let b = Buffer.create 80 in
        String.iter
          (function
            | '\\' -> Buffer.add_char b '/'
            | c -> Buffer.add_char b c
          )
          s;
        Buffer.contents b
    | _ ->
	s


let out_path ?(prefix="") s =
  match Findlib_config.system with
    | "mingw" | "mingw64" | "cygwin" ->
	let u = slashify s in
	prefix ^ 
	  (if String.contains u ' ' then
	     (* Desperate attempt to fix the space problem in paths.
                Note that we invoke commands via Unix.open_process, and
                this function already quotes the arguments on win32.
                However, for -ccopt arguments, one quoting level seems
                to be lost, and we have to add another level to compensate.
                E.g. for the list of args
                  [ -ccopt; -L/my programs/include -L/somewhere ]
                we get after out_path
                  [ -ccopt; "-I/my programs/include -L/somewhere" ]
                which actually translates to
                  -ccopt "\"-I/my programs/include\" \"-L/somewhere\""
                on the command line, i.e. a double-quoted argument.
	      *)
	     "\"" ^ u ^ "\""
	   else
	     u
	  )
    | _ ->
	prefix ^ slashify s



let percent_subst ?base spec lookup s =
  (* spec = [ "%c", [ "ctext1"; "ctext2"; ... ];
   *          "%d", [ "dtext1"; "dtext2"; ... ] ]
   * All occurrences of %c in the string s are replaced as specified in spec.
   * spec is an association list with the %-notation as keys
   * and lists of strings as values. The result is a list of strings containing
   * every combination of substituted values.
   *
   * Support for the %(name) syntax: In this case, the name is taken as
   * key for the [lookup] function, which either returns the string value
   * or raises Not_found.
   *
   * "+" modifier: A "+" after "%" causes that Findlib.resolve_path is
   * called for the substitution string (e.g. %+c, %+(name)).
   *
   * Example:
   * spec = [ "%a", [ "file1" ] ]
   * lookup = function "archive" -> "file2" | _ -> raise Not_found
   * Here, %a is substituted by file1, and %(archive) is substituted by
   * file2.
   *
   * ?base: The base parameter for Findlib.resolve_path.
   *)
  let l = String.length s in

  let fail() =
    failwith "bad format string" in

  let parenthesized_name j =
    try		  
      if j+1>=l then raise Not_found;
      let k = String.index_from s (j+1) ')' in
      let name = String.sub s (j+1) (k-j-1) in
      (name, k+1)
    with Not_found ->
      fail() in

  let rec preprocess i j =
    if j<l then begin
      match s.[j] with
	'%' ->
	  if j+1<l then begin
	    let prev = Const(String.sub s i (j-i)) in
	    let c = s.[j+1] in
	    match c with
		'%' -> 
		  prev :: Const "%" :: preprocess (j+2) (j+2)
	      | '(' ->
                  let name, j_next = parenthesized_name (j+1) in
                  prev :: Lookup(name,Plain) :: preprocess j_next j_next
              | '+' ->
                  if j+2<l then begin
                    let c = s.[j+2] in
                    match c with
                      | '%' | '+' -> fail()
                      | '(' ->
                           let name, j_next = parenthesized_name (j+2) in
                           prev :: Lookup(name,Plus) :: preprocess j_next j_next
                      | _ ->
		           let name = "%" ^ String.make 1 c in
	                   prev :: Percent(name,Plus) :: preprocess (j+3) (j+3)
                  end
                  else fail()
	      | _ ->
		  let name = "%" ^ String.make 1 c in
		  prev :: Percent(name,Plain) :: preprocess (j+2) (j+2)
	  end
	  else fail()
      |	_ ->
	  preprocess i (j+1)
    end
    else
      if i<j then
	[Const(String.sub s i (j-i))]
      else
	[]
  in

  let plus_subst u =
    String.concat
      " "
      (List.map
         (Findlib.resolve_path ?base)
         (Fl_split.in_words u)) in

  let any_subst modi u =
    match modi with
      | Plain -> u
      | Plus -> plus_subst u in

  let rec subst prefix l =
    match l with
      [] -> [prefix]
    | Const s :: l' ->
	subst (prefix ^ s) l'
    | Percent(name,modi) :: l' ->
	let replacements0 =
	  try List.assoc name spec
	  with Not_found -> failwith "bad format string" in
        let replacements =
          List.map (any_subst modi) replacements0 in
	List.flatten
	  (List.map
	     (fun replacement ->
	       subst (prefix ^ replacement) l')
	     replacements)
    | Lookup(name,modi) :: l' ->
	let replacement0 =
	  try lookup name
	  with Not_found -> "" in
        let replacement =
          any_subst modi replacement0 in
	subst (prefix ^ replacement) l'
  in

  subst "" (preprocess 0 0)
;;


let rec remove_dups l =
  match l with
    x :: l' ->
      if List.mem x l' then remove_dups l' else x::remove_dups l'
  | [] -> []
;;

let current = ref 0
let argv = ref [||]

let arg n =
  if n < Array.length !argv then (!argv).(n) else raise Not_found
;;


let escape_if_needed s =
  if String.contains s ' ' then "\"" ^ String.escaped s ^ "\"" else s
;;


let use_package prefix pkgnames =
  (* may raise No_such_package *)
  let pdirs =
    List.map
      (fun pname ->
         "-I " ^ out_path(package_directory pname)
      )
      pkgnames
  in

  print_endline (prefix ^ String.concat " " pdirs)
;;


let read_ldconf filename =
  let lines = ref [] in
  let f = open_in filename in
  try
    while true do
      let line = input_line f in
      if line <> "" then
	lines := line :: !lines
    done;
    assert false
  with
      End_of_file ->
	close_in f;
	List.rev !lines
    | other ->
	close_in f;
	raise other
;;


let format_ldconf lines new_lines =
  let b = Buffer.create 23 in
  List.iter
    (fun line -> Buffer.add_string b (line ^ "\n"))
    (lines @ new_lines) ;
  Buffer.to_bytes b
;;


let write_ldconf filename lines new_lines =
  let f = open_out filename in
  try
    List.iter
      (fun line -> output_string f (line ^ "\n"))
      (lines @ new_lines);
    close_out f;
    prerr_endline("Updated " ^ filename);
  with
      Sys_error e ->
	prerr_endline ("ocamlfind: [WARNING] Cannot write " ^ filename);
	prerr_endline ("Reason: " ^ e);
	prerr_endline ("This file contains the directories with DLLs.");
	if new_lines <> [] then begin
	  prerr_endline ("It is recommended to add the following line(s) to this file:");
	  List.iter prerr_endline new_lines
	end
;;


let is_dll p =
  let sfx = Findlib_config.dll_suffix in
  sfx <> "" && Filename.check_suffix p sfx
;;


let identify_dir d =
  match Sys.os_type with
    | "Win32" ->
	failwith "identify_dir"   (* not available *)
    | _ ->
	let s = Unix.stat d in
	(s.Unix.st_dev, s.Unix.st_ino)
;;


let conflict_report incpath pkglist =
  (* Check whether there are several definitions for packages
   * in the current path. We remove duplicate directories first.
   * Note that all other checks are not sensitive to duplicate directories.
   *)
  Fl_package_base.package_conflict_report ~identify_dir ();

  (* Second check whether there are module conflicts *)
  let pkgpath =
    List.map Findlib.package_directory pkglist in
  Fl_package_base.module_conflict_report ~identify_dir (pkgpath @ incpath);

  (* Finally check whether there are multiple DLLs: *)
  (* Note: Only the directories mentioned in ld.conf are checked, but not the
   * directories in [incpath], and not the directories in CAML_LD_LIBRARY_PATH.
   * The idea of this check is to ensure a proper installation, and not to
   * complain about the user's special configuration.
   *)
  let ldconf = ocaml_ldconf() in
  if ldconf <> "ignore" then begin
    let dll_dirs = remove_dups (read_ldconf ldconf) in
    let dll_pairs =
      List.flatten
	(List.map
	   (fun dll_dir ->
	      let files =
		try Array.to_list (Sys.readdir dll_dir)
		with _ ->
		  prerr_endline ("ocamlfind: [WARNING] Cannot read directory " ^
				 dll_dir ^ " which is mentioned in ld.conf");
		  []
	      in
	      List.map
		(fun file -> (file, dll_dir))
		(List.filter is_dll files)
	   )
	   dll_dirs
	) in
    let dll_hash = Hashtbl.create 50 in
    List.iter
      (fun (file, dll_dir) -> Hashtbl.add dll_hash file dll_dir)
      dll_pairs;
    Hashtbl.iter
      (fun file dll_dir ->
	 let locations = Hashtbl.find_all dll_hash file in
	 if List.length locations > 1 then begin
	   prerr_endline ("ocamlfind: [WARNING] The DLL " ^ file ^
			  " occurs in multiple directories: " ^ dll_dir)
	 end
      )
      dll_hash
  end
;;


let check_package_list l =
  (* may raise No_such_package *)
  List.iter
    (fun pkg ->
       let _ = package_directory pkg in
       ()
    )
    l
;;


type verbosity =
  | Normal
  | Verbose
  | Only_show


let run_command ?filter verbose cmd args =
  let printable_cmd =
    cmd ^ " " ^ String.concat " " (List.map escape_if_needed args) in
  ( match verbose with
      | Normal ->
          ()
      | Verbose ->
          print_endline ("+ " ^ printable_cmd);
          if filter <> None then
            print_string
              ("  (output of this command is filtered by ocamlfind)\n")
      | Only_show ->
          print_endline printable_cmd
  );
  flush stdout;

  if verbose <> Only_show then (
    let filter_input, cmd_output =
      match filter with
          None -> Unix.stdin (* dummy *), Unix.stdout
        | Some f -> Unix.pipe()
    in

    (* Signals: On SIGINT, we wait until the subprocess finishes, and
     * die then. This allows us to call interactive commands as subprocesses.
     *)

    let old_sigint =
      Sys.signal Sys.sigint Sys.Signal_ignore in

    let need_exe =
      List.mem Findlib_config.system [ "win32"; "win64"; "mingw"; "mingw64" ] in

    let fixed_cmd =
      if need_exe then (
        if Filename.check_suffix cmd ".exe" then cmd else cmd ^ ".exe" 
      )
      else
        cmd in

    let pid =
      Unix.create_process
        fixed_cmd
        (Array.of_list (cmd :: args))
        Unix.stdin
        cmd_output
        Unix.stderr
    in

    begin match filter with
        Some filter_fun ->
          begin
            Unix.close cmd_output;
            let ch = Unix.in_channel_of_descr filter_input in
            try
              while true do
                let line = input_line ch in
                match filter_fun line with
                    None -> ()       (* Suppress line *)
                  | Some line' -> print_endline line'
              done;
              assert false
            with
                End_of_file ->
                  close_in ch;
                  flush stdout
          end
      | None -> ()
    end;

    let (_,status) = Unix.waitpid [] pid in
    Sys.set_signal Sys.sigint old_sigint;
    begin
      match status with
        Unix.WEXITED 0 -> ()
      | Unix.WEXITED n ->
          if verbose = Verbose then
            print_string (cmd ^ " returned with exit code " ^ string_of_int n ^ "\n");
          exit n
      | Unix.WSIGNALED _ ->
          print_string (cmd ^ " got signal and exited\n");
          exit 2
      | Unix.WSTOPPED _ ->
          failwith "Your operating system does not work correctly"
    end
  )
;;


(**************** preprocessor ******************************************)

let select_pp_packages syntax_preds packages =
  if syntax_preds = [] then
    (* No syntax predicates, no preprocessor! *)
    []
  else
    List.filter
      (fun pkg ->
         let al = try package_property syntax_preds pkg "archive"
	          with Not_found -> "" in
         let w = Fl_split.in_words al in
	 w <> []
      )
      packages


let process_pp_spec syntax_preds packages pp_opts =
  (* Returns: pp_command *)
  (* may raise No_such_package *)

  (* [packages]: all packages given on the command line. May include
   * packages for compilation and for preprocessing.
   *
   * The difficulty is now that the preprocessor packages may have
   * requirements that are non-preprocessor packages. To get exactly
   * the preprocessor packages and its requirements, we do:
   *
   * 1. Determine the subset of [packages] that are preprocessor
   *    packages by checking whether they have an "archive" for
   *    [syntax_preds], i.e. the preprocessor packages mentioned
   *    on the command line = [cl_pp_packages].
   *
   * 2. Add their requirements = [pp_packages]
   *
   * Because the packages are now mixed, we must evaluate for 
   * [syntax_preds] + "byte".
   *)

  (* One packages must now have the variable "preprocessor", usually camlp4 *)
  let cl_pp_packages = select_pp_packages syntax_preds packages in
  let pp_packages =
    package_deep_ancestors syntax_preds cl_pp_packages in

  let preprocessor_cmds =
    List.flatten
      (List.map (fun pname ->
		   try
		     [ pname,
		       package_property syntax_preds pname "preprocessor"
		     ]
		   with
 		       Not_found -> []
		)
	        pp_packages
      )
  in

  let preprocessor_cmd =
    if syntax_preds <> [] then
      match preprocessor_cmds with
	  [] ->
	    failwith("Using -syntax, but no package is selected specifying \
                     a preprocessor as required for -syntax")
	| [_, cmd] -> Some cmd
	| _ ->
	    failwith("Several packages are selected that specify \
                      preprocessors: " ^ 
		       String.concat ", "
		      (List.map
			 (fun (n,v) ->
			    "package " ^ n ^ " defines `" ^ v ^ "'")
			 preprocessor_cmds
		      )
		    )
    else
      None
  in

  let pp_i_options =
    List.flatten
      (List.map
	 (fun pkg ->
	    let pkgdir = package_directory pkg in
	      [ "-I"; slashify pkgdir ]
	 )
	 pp_packages) in

  let pp_archives =
    if preprocessor_cmd = None then
      []
    else
      List.flatten
	(List.map
	   (fun pkg ->
	      let al = 
		try package_property ("byte" :: syntax_preds) pkg "archive"
	        with Not_found -> "" in
	      Fl_split.in_words al
	   )
	   pp_packages) in

  match preprocessor_cmd with
      None -> []
    | Some cmd ->
      let cmd = Str.(split (regexp "[ \t]+") cmd) in
	 cmd@
	 pp_i_options@
	 pp_archives@
	 pp_opts
;;

(**************** ppx extensions ****************************************)

let process_ppx_spec predicates packages ppx_opts =
  (* Returns: ppx_commands *)
  (* may raise No_such_package *)

  let ppx_packages =
    package_deep_ancestors predicates packages in

  let ppx_opts =
    List.map
      (fun opt ->
         match Fl_split.in_words opt with
           | pkg :: ((_ :: _) as opts) ->
               let exists =
                 try ignore(package_directory pkg); true
                 with No_such_package _ -> false in
               if not exists then
                 failwith ("The package named in -ppxopt does not exist: " ^
                             pkg);
               pkg, opts
           | _ ->
               failwith "-ppxopt must include package name, e.g. -ppxopt \"foo,-name bar\""
      )
      ppx_opts in

  let meta_ppx_opts =
    List.concat
      (List.map
        (fun pname ->
          try
            let opts = package_property predicates pname "ppxopt" in
            (* Split by whitespace to get (package,options) combinations.
               Then, split by commas to get individual options. *)
            List.map
              (fun opts ->
                match Fl_split.in_words opts with
                | pkg :: ((_ :: _) as opts) ->
                    let exists =
                      try ignore(package_directory pkg); true
                      with No_such_package _ -> false in
                    if not exists then
                      failwith ("The package named in ppxopt variable does not exist: " ^
                                  pkg ^ " (from " ^ pname ^ ")");
                    let base = package_directory pname in
                    pkg, List.map (resolve_path ~base ~explicit:true) opts
                | _ ->
                    failwith ("ppxopt variable must include package name, e.g. " ^
                              "ppxopt=\"foo,-name bar\" (from " ^ pname ^ ")")
              )
              (Fl_split.in_words_ws opts)
          with Not_found -> []
        )
        ppx_packages
      ) in

  List.flatten
    (List.map
       (fun pname ->
          let base = package_directory pname in
          let options =
            try
              List.concat
                (List.map (fun (_, opts) -> opts)
                  (List.filter (fun (pname', _) -> pname' = pname)
                    (meta_ppx_opts @ ppx_opts)))
            with Not_found -> []
          in
          try
            let preprocessor =
              resolve_path
                ~base ~explicit:true
                (package_property predicates pname "ppx") in
            ["-ppx"; String.concat " " (preprocessor :: options)]
          with Not_found -> []
       )
       ppx_packages)

let parse_args
      ?(current = current) ?(args = !argv) 
      ?(align = true)
      spec anon usage =
  try
    Arg.parse_argv
      ~current
      args
      (if align then Arg.align spec else spec)
      anon
      usage
  with
    | Arg.Help text ->
        print_string text;
        exit 0
    | Arg.Bad text ->
        prerr_string text;
        exit 2  




(**************** OCAMLC/OCAMLMKTOP/OCAMLOPT subcommands ****************)

let ppx_invoke1 cmd ~root f =
  let open Filename in
  let outf = temp_file root "" in
  (Printf.sprintf "%s %s %s" cmd f outf, outf)
;;

let ppx_invoke cmds f =
  let open Filename in
  let base = basename f in
  if not (check_suffix base ".mli" || check_suffix base ".ml") then
    failwith "File must end with either .ml or .mli" ;
  let (root, suff) =
    if check_suffix base ".mli" then
      (chop_suffix base ".mli", ".mli")
        else (chop_suffix base ".ml", ".ml") in

  let extra_arg = if suff = ".mli" then "-intf" else "-impl" in
  let outf0 = temp_file root "" in
  let cmd0 = Printf.sprintf "ocamlfind ocamlfind2/papr_official.exe -binary-output %s %s %s" extra_arg f outf0 in
  let (outf, cmdsacc, tmpfiles) =
    List.fold_left (fun (inf, cmdsacc, tmpfiles) cmd ->
        let (cmd, outf) = ppx_invoke1 ~root cmd inf in
        (outf, ((cmd,outf)::cmdsacc), outf::tmpfiles))
      (outf0, [(cmd0, outf0)], [outf0]) cmds in
  let cmds = List.rev cmdsacc in
  (suff, cmds, outf, tmpfiles)
;;

let check_rc msg cmd =
  Printf.fprintf stderr "%s: %s\n%!" msg cmd ;
  match Unix.system cmd with
    Unix.WEXITED 0 -> ()
  | Unix.WEXITED n ->
    Printf.fprintf stderr "%s: process exited with status %d\n%!" msg n ;
        failwith (Printf.sprintf "%s: failed" msg)
  | _ -> failwith (Printf.sprintf "%s: failed with unexpected status" msg)
;;

let ppx_execute (suff, cmds, outf, tmpfiles) =
  cmds |> List.iter (fun (cmd, outf) ->
      check_rc "ppx_execute" cmd ;
    ) ;
  let extra_arg = if suff = ".mli" then "-intf" else "-impl" in
  check_rc "format output file"
    (Printf.sprintf "ocamlfind ocamlfind2/papr_official.exe -binary-input %s %s" extra_arg outf)
(*
  check_rc "unlink tmpfiles" (Printf.sprintf "rm -f %s" (String.concat " " tmpfiles))
*)
;;

type pass_file_t =
    Pass of string
  | Impl of string   (* Forces module implementation: -impl <file> *)
  | Intf of string   (* Forces module interface: -intf <file> *)
  | Cclib of string  (* Option for the C linker: -cclib <opt> *)
;;


let contracted_ocamlmklib_options =
  [ "-l"; "-L"; "-R"; "-F"; "-Wl,-rpath,"; "-Wl,-R" ]
    (* The ocamlmklib options where the argument is directly attached to the
       switch (e.g. -L<path> instead of -L <path>)
     *)


let preprocess () =

  (* let destdir = ref (default_location()) in *)
  
  let pass_files = ref [] in

  let packages = ref [] in
  let predicates = ref [] in

  let syntax_preds = ref [] in
  let pp_opts = ref [] in
  let ppx_opts = ref [] in
  let verbose = ref Normal in

  let add_pkg =
    Arg.String (fun s -> packages := !packages @ (Fl_split.in_words s)) in
  let add_pred =
    Arg.String (fun s -> predicates := !predicates @ (Fl_split.in_words s)) in
  let add_syntax_pred =
    Arg.String (fun s -> syntax_preds := !syntax_preds @ (Fl_split.in_words s)) in
  let add_pp_opt =
    Arg.String (fun s -> pp_opts := !pp_opts @ [s]) in
  let ignore_error = ref false in

  let arg_spec =
    [
          "-package", add_pkg,
            "<name>   Refer to package when compiling";
          "-predicates", add_pred,
            "<p>   Add predicate <p> when resolving package properties";
          "-syntax", add_syntax_pred,
            "<p>       Use preprocessor with predicate <p>";
          "-ppopt", add_pp_opt,
            "<opt>      Append option <opt> to preprocessor invocation";
          "-ppxopt", Arg.String (fun s -> ppx_opts := !ppx_opts @ [s]),
            "<pkg>,<opts>  Append options <opts> to ppx invocation for package <pkg>";
          "-ignore-error", Arg.Set ignore_error,
            "     Ignore the 'error' directive in META files";
          "-only-show", Arg.Unit (fun () -> verbose := Only_show),
            "         Only show the constructed command, but do not exec it\nSTANDARD OPTIONS:";
          "-verbose", Arg.Unit (fun () -> verbose := Verbose),
            "         Only show the constructed command, but do not exec it\nSTANDARD OPTIONS:";
        ] in

  let (current,args) =
      (current, !argv) in

  parse_args
    ~current
    ~args
    arg_spec
    (fun s -> pass_files := !pass_files @ [ Pass s])
    ("usage: ocamlfind2 preprocess [options] file ...");

  (* ---- Start requirements analysis ---- *)
  
  predicates := "byte" :: !predicates;

  if !syntax_preds <> [] then begin
    predicates := "syntax" :: !predicates;
    syntax_preds := "preprocessor" :: "syntax" :: !syntax_preds;
  end;

  (* check packages: *)
  check_package_list !packages;

  let eff_packages =
    package_deep_ancestors !predicates !packages in

  (* ---- End of requirements analysis ---- *)

  (* Add the pkg_<name> predicates: *)
  predicates := List.map (fun pkg -> "pkg_" ^ pkg) eff_packages @ !predicates;

  (* Check on [warning] directives: *)
  List.iter
    (fun pkg ->
       try
         let warning = package_property !predicates pkg "warning" in
         prerr_endline("ocamlfind: [WARNING] Package `" ^ pkg ^
                         "': " ^ warning)
       with
	   Not_found -> ()
    )
    eff_packages;

  (* Check on [error] directives: *)
  List.iter
    (fun pkg ->
       try
	 let error = package_property !predicates pkg "error" in
	 if !ignore_error then
	   prerr_endline("ocamlfind: [WARNING] Package `" ^ pkg ^
			 "' signals error: " ^ error)
	 else
	   failwith ("Error from package `" ^ pkg ^ "': " ^ error)
       with
	   Not_found -> ()
    )
    eff_packages;

  if !verbose = Verbose then begin
    if !syntax_preds <> [] then
      print_string ("Effective set of preprocessor predicates: " ^
		    String.concat "," !syntax_preds ^ "\n");
    print_string ("Effective set of compiler predicates: " ^
		  String.concat "," !predicates ^ "\n");
  end;

  (* initl_file_name: the initialization code inserted at the end of
   *   the cma/cmo list (initl = init last)
   *)


  let pp_command =
      process_pp_spec !syntax_preds !packages !pp_opts
  in

  let ppx_commands =
    process_ppx_spec !predicates !packages !ppx_opts
  in

  let pass_files' =
    List.flatten
      (List.map
	 (function
	      Pass s ->
		if s <> "" && s.[0] = '-'
		then [ "-"; String.sub s 1 (String.length s - 1) ]
		else [ resolve_path s ]
	    | Impl s ->
		[ "-impl"; resolve_path s ]
	    | Intf s ->
		[ "-intf"; resolve_path s ]
	    | Cclib s ->
		[ "-cclib"; s ]
	 )
	 !pass_files)
  in

  if pp_command <> [] && ppx_commands <> [] then
    prerr_endline("ocamlfind2: [ERROR] both pp and ppx commands present (cannot preprocess)") ;

  if pp_command <> [] then
    let pp_command = pp_command@ pass_files' in
    run_command !verbose (List.hd pp_command) (List.tl pp_command)
  else begin
    let ppx_commands = except "-ppx" ppx_commands in
    List.iter (fun f ->
        let (suff, cmds, outf, tmpfiles) = ppx_invoke ppx_commands f in
        ppx_execute (suff, cmds, outf, tmpfiles)
      ) pass_files' ;

  end
;;

(************************************************************************)


let copy_file ?(rename = (fun name -> name)) ?(append = "") src dstdir =
  (* A system-independent function to copy the file src to dstdir *)
  let outname = rename (Filename.basename src) in
  let ch_in = open_in_bin src in
  (* Determine the permissions of the file: the permissions of the
   * user bits are extended to all groups (user, group, world bits),
   * and the umask is applied to the result.
   * Furthermore, the mtime of the file is preserved. This seems to be
   * important for BSD-style archives (otherwise the system is confused
   * and wants that ranlib is run again). For simplicity, the atime is
   * set to the mtime, too.
   *)
  let s = Unix.stat src in
  let perm = s.Unix.st_perm in
  let user_perm = (perm land 0o700) lsr 6 in
  let perm' = user_perm lor (user_perm lsl 3) lor (user_perm lsl 6) in
  try
    let outpath = Filename.concat dstdir outname in
    if Sys.file_exists outpath then
      prerr_endline ("ocamlfind: [WARNING] Overwriting file " ^ outpath);
    let ch_out = open_out_gen
		   [Open_wronly; Open_creat; Open_trunc; Open_binary]
		   perm'
		   outpath in
    try
      let buflen = 4096 in
      let buf = Bytes.create buflen in   (* FIXME: Bytes.create *)
      let pos = ref 0 in
      let len = ref (input ch_in buf 0 buflen) in
      while !len > 0 do
	output ch_out buf !pos !len;
	len := input ch_in buf !pos buflen;
      done;
      output_string ch_out append;
      close_out ch_out;
      close_in ch_in;
      Unix.utimes outpath s.Unix.st_mtime s.Unix.st_mtime;

      prerr_endline("Installed " ^ outpath);
    with
	exc -> close_out ch_out; raise exc
  with
      exc -> close_in ch_in; raise exc
;;


let install_create_directory pkgname dstdir =
  try
    Unix.mkdir dstdir 0o777
  with
      Unix.Unix_error(Unix.EEXIST,_,_) ->
	()
    | Unix.Unix_error(Unix.ENOENT,_,_)
    | Unix.Unix_error(Unix.ENOTDIR,_,_) ->
	failwith ("Bad configuration: Cannot mkdir " ^ dstdir ^ " because a path component does not exist or is not a directory")
    | Unix.Unix_error(e,_,_) ->
	failwith ("Cannot mkdir " ^ dstdir ^ ": " ^
		  Unix.error_message e)
;;


let create_owner_file pkg file =
  let outpath = file ^ ".owner" in
  let f = open_out outpath in
  try
    output_string f (pkg ^ "\n");
    close_out f;
    prerr_endline("Installed " ^ outpath);
  with
      exc -> close_out f; raise exc
;;

let trim_cr s =
  let len = String.length s in
  if len > 0 && String.get s (len-1) = '\r' then
    String.sub s 0 (len-1)
  else
    s

let find_owned_files pkg dir =
  let files = Array.to_list(Sys.readdir dir) in
  List.filter
    (fun file ->
       let owner_file =
	 if Filename.check_suffix file ".owner" then
	   file
	 else
	   file ^ ".owner" in
       (List.mem owner_file files) && (
         try
           let fd =
             Unix.openfile (Filename.concat dir owner_file) [Unix.O_RDONLY] 0 in
           let f =
             Unix.in_channel_of_descr fd in
           try
             let line = trim_cr (input_line f) in
             let is_my_file = (line = pkg) in
             close_in f;
             is_my_file
           with
             | End_of_file -> close_in f; false
             | exc -> close_in f; raise exc
         with
           | Unix.Unix_error(Unix.ENOENT,_,_) ->
               (* the owner file might have been removed by a package
                  removal that is being done in parallel
                *)
               false
           | Unix.Unix_error(code, _, arg) ->
               raise(sys_error code arg)
       )
    )
    files
;;



exception Missing_archives of Fl_metascanner.pkg_expr

let rec patch_archives pkgdir pkg =
  (* First remove all missing files from archive variables: *)
  let defs' =
    List.map
      (fun def ->
	 if def.Fl_metascanner.def_var = "archive" then (
	   let files = Fl_split.in_words def.Fl_metascanner.def_value in
	   let files' =
	     List.filter
	       (fun file -> 
		  let p = Findlib.resolve_path ~base:pkgdir file in
		  Sys.file_exists p)
	       files in
	   { def with
	       Fl_metascanner.def_value = String.concat " " files'
	   }
	 )
	 else def
      )
      pkg.Fl_metascanner.pkg_defs in
  (* Remove empty archive variables: *)
  let defs'' =
    List.filter
      (fun def ->
	 def.Fl_metascanner.def_var <> "archive" ||
	 Fl_split.in_words def.Fl_metascanner.def_value <> []
      )
      defs' in
  (* Return the package or raise Not_found if all archives vanished: *)
  let children = 
    (* Recursive patch, remove all Not_found packages: *)
    List.flatten
      (List.map
	 (fun (name, child) ->
	    try [ name, patch_archives pkgdir child ]
	    with Missing_archives _ -> []
	 )
	 pkg.Fl_metascanner.pkg_children) in
  let pkg' =
    { Fl_metascanner.pkg_defs = defs'';
      pkg_children = children
    } in
  if List.exists (fun def -> def.Fl_metascanner.def_var = "archive") defs'' then
    pkg'
  else
    raise (Missing_archives pkg')
;;


let rec patch_pkg pkgdir pkg patches =
  match patches with
    | [] -> pkg
    | (`Version v) :: patches' ->
	let def =
	  { Fl_metascanner.def_var = "version";
	    def_flav = `BaseDef;
	    def_preds = [];
	    def_value = v 
	  } in
	let defs =
	  List.filter
	    (fun d -> d.Fl_metascanner.def_var <> "version")
	    pkg.Fl_metascanner.pkg_defs in
	let pkg' =
	  { pkg with
	      Fl_metascanner.pkg_defs = def :: defs
	  } in
	patch_pkg pkgdir pkg' patches'
    | (`Rmpkg n) :: patches' ->
	let children =
	  List.filter
	    (fun (name,_) -> name <> n)
	    pkg.Fl_metascanner.pkg_children in
	let pkg' =
	  { pkg with
	      Fl_metascanner.pkg_children = children
	  } in
	patch_pkg pkgdir pkg' patches'
    | `Archives :: patches' ->
	let pkg' = 
	  try patch_archives pkgdir pkg 
	  with
	      Missing_archives p -> p in
	patch_pkg pkgdir pkg' patches'
;;


exception Skip_file;;

type which = Auto | Dll | No_dll;;

let meta_pkg meta_name =
  let f = open_in meta_name in
  try
    let pkg = Fl_metascanner.parse f in
    close_in f;
    pkg
  with
  | Failure s
  | Stream.Error s ->
    close_in f;
    failwith ("Cannot parse '" ^ meta_name ^ "': " ^ s)

let char_lowercase_ascii c =
  (* Char.lowercase_ascii and String.lowercase_ascii first available in
     OCaml-4.03, but we want to support earlier versions too
   *)
  if (c >= 'A' && c <= 'Z')
  then Char.unsafe_chr(Char.code c + 32)
  else c

let string_lowercase_ascii =
  String.map char_lowercase_ascii


let reinstall_if_diff_package () =
  let destdir = ref (default_location()) in
  let metadir = ref (meta_directory()) in
  let ldconf  = ref (ocaml_ldconf()) in
  let don't_add_directory_directive = ref false in
  let pkgname = ref "" in
  let auto_files = ref [] in
  let dll_files = ref [] in
  let nodll_files = ref [] in
  let which = ref Auto in
  let add_files = ref false in
  let optional = ref false in
  let patches = ref [] in

  let keywords =
    [ "-destdir", (Arg.String (fun s -> destdir := s)),
              ("<path>    Set the destination directory (default: " ^
	       !destdir ^ ")");
      "-metadir", (Arg.String (fun s -> metadir := s)),
              ("<path>    Install the META file into this directory (default: "^
	       (if !metadir = "" then "none" else !metadir) ^ ")");
      "-ldconf", (Arg.String (fun s -> ldconf := s)),
             ("<path>     Update this ld.conf file (default: " ^ !ldconf ^ ")");
      "-dont-add-directory-directive", (Arg.Set don't_add_directory_directive),
                                    " never append directory='...' to META";
      "-dll", Arg.Unit (fun () -> which := Dll),
           "              The following files are DLLs";
      "-nodll", Arg.Unit (fun () -> which := No_dll),
             "            The following files are not DLLs";
      "-add", Arg.Unit (fun () -> add_files := true),
           "              Add files to the package";
      "-optional", Arg.Set optional,
                "         The following files are optional";
      "-patch-version", Arg.String (fun s -> patches := !patches @ [`Version s]),
                     "<v> Set the package version to <v>";
      "-patch-rmpkg", Arg.String (fun s -> patches := !patches @ [`Rmpkg s]),
                   "<n>   Remove the subpackage <n>";
      "-patch-archives", Arg.Unit (fun () -> patches := !patches @ [`Archives]),
                      "   Remove non-existing archives";
    ] in
  let errmsg = "usage: ocamlfind install [options] <package_name> <file> ..." in

  parse_args
        keywords
	(fun s ->
	   if !pkgname = ""
	   then pkgname := s
	   else
	     if not !optional || Sys.file_exists s then
	       match !which with
		   Auto -> auto_files := s :: !auto_files
		 | Dll  -> dll_files := s :: !dll_files
		 | No_dll -> nodll_files := s :: !nodll_files
	)
	errmsg;
  if !pkgname = "" then (Arg.usage keywords errmsg; exit 1);
  if not (Fl_split.is_valid_package_name !pkgname) then
    failwith "Package names must not contain the character '.'!";

  let pkgdir = Filename.concat !destdir !pkgname in
  let dlldir = Filename.concat !destdir Findlib_config.libexec_name in
  let has_metadir = !metadir <> "" in
  let meta_dot_pkg = "META." ^ !pkgname in

  (* The list of all files to install: *)
  let full_list  = !auto_files @ !dll_files @ !nodll_files in
  (* Check whether there are DLLs: *)
  let (l1,l2)    = List.partition is_dll !auto_files in
  let dll_list   = l1 @ !dll_files in
  let nodll_list = l2 @ !nodll_files in
  let have_libexec = Sys.file_exists dlldir in
  let pkgdir_list = if have_libexec then nodll_list else full_list in
  let pkgdir_eff_list =
    (* The files that will be placed into pkgdir: *)
    List.map
      (fun f ->
	 if f = meta_dot_pkg then "META" else f)
      (List.filter
	 (fun f ->
	    not has_metadir ||
	      (f <> "META" && f <> meta_dot_pkg))
	 pkgdir_list) in
  
  (* Check whether META exists: (And check syntax) *)
  let meta_name =
    try
      List.find
	(fun p ->
	   let b = Filename.basename p in
	   b = "META" || b = meta_dot_pkg)
	nodll_list
    with
      | Not_found ->
	  if !add_files then (
	    let m1 = Filename.concat !metadir meta_dot_pkg in
	    let m2 = Filename.concat pkgdir "META" in
	    if Sys.file_exists m1 then
	      m1
	    else
	      if Sys.file_exists m2 then
		m2
	      else
		failwith "Cannot find META in package dir"
	  )
	  else
	    failwith "The META file is missing" in

  let meta_pkg = meta_pkg meta_name in

  if not !add_files then (
    (* Check for frequent reasons why installation can go wrong *)
    if Sys.file_exists (Filename.concat !metadir meta_dot_pkg) then
      failwith ("Package " ^ !pkgname ^ " is already installed\n - (file " ^ Filename.concat !metadir meta_dot_pkg ^ " already exists)");

    if Sys.file_exists (Filename.concat pkgdir "META") then
      failwith ("Package " ^ !pkgname ^ " is already installed\n - (file " ^ pkgdir ^ "/META already exists)");
  );
  List.iter
    (fun f ->
       let f' = Filename.concat pkgdir f in
       if Sys.file_exists f' then
	 failwith ("Conflict with file: " ^ f'))
    pkgdir_eff_list;

  if have_libexec then begin
    List.iter
      (fun dll ->
	 let b = Filename.basename dll in
	 if Sys.file_exists (Filename.concat dlldir b) then
	   failwith ("Conflict with another package: Library " ^ b ^
		     " is already installed");
      )
      dll_list
  end;

  (* Create the package directory: *)
  install_create_directory !pkgname pkgdir;

  (* Now copy the files into the package directory (except META): *)
  List.iter
    (fun p ->
       try
	 copy_file
	   ~rename: (fun f ->
			 if f = "META" || f = meta_dot_pkg then 
			   raise Skip_file
			 else
			   f)
	   p
	   pkgdir
       with
	   Skip_file -> ()
    )
    pkgdir_list;

  (* Copy the DLLs into the libexec directory if necessary *)
  if have_libexec then begin
    List.iter
      (fun p ->
	 copy_file p dlldir;
	 create_owner_file !pkgname
	   (Filename.concat dlldir (Filename.basename p))
      )
      dll_list
  end;

  (* Extend ld.conf if necessary: *)
  if dll_list <> [] && !ldconf <> "ignore" && not have_libexec then begin
    if Sys.file_exists !ldconf then
      begin
	let lines = read_ldconf !ldconf in
	write_ldconf !ldconf lines [ pkgdir ]
      end
    else
      prerr_endline("ocamlfind: [WARNING] You have installed DLLs but there is no ld.conf")
  end;

  if dll_list <> [] && have_libexec && !ldconf <> "ignore" then begin
    (* Check whether libexec is mentioned in ldconf *)
    (* FIXME: We have to be careful with case-insensitive filesystems. 
       Currently, we only check for Win32, but also OS X may have ci 
       filesystems. So some better check would be nice.
     *)
    let lines = read_ldconf !ldconf in
    let dlldir_norm = Fl_split.norm_dir dlldir in
    let dlldir_norm_lc = string_lowercase_ascii dlldir_norm in
    let ci_filesys = (Sys.os_type = "Win32") in
    let check_dir d =
      let d' = Fl_split.norm_dir d in
      (d' = dlldir_norm) || 
        (ci_filesys && string_lowercase_ascii d' = dlldir_norm_lc) in
    if not (List.exists check_dir lines) then
      prerr_endline("ocamlfind: [WARNING] You have installed DLLs but the directory " ^ dlldir_norm ^ " is not mentioned in ld.conf");
  end;

  (* Finally, write the META file: *)
  let write_meta append_directory dir name =
    (* If there are patches, write the patched META, else copy the file: *)
    if !patches = [] then
      copy_file 
	~rename:(fun _ -> name)
        ?append:(if append_directory then
		   Some("\ndirectory=\"" ^ pkgdir ^ 
			  "\" # auto-added by ocamlfind\n")
		 else
		   None)
	meta_name
	dir
    else (
      let p = Filename.concat dir name in
      let patched_pkg = patch_pkg pkgdir meta_pkg !patches in
      let out = open_out p in
        Fl_metascanner.print out patched_pkg;
      if append_directory then
	output_string out ("\ndirectory=\"" ^ pkgdir ^ 
			     "\" # auto-added by ocamlfind\n");
      close_out out;
      prerr_endline ("Installed " ^ p);
    )
  in
  if not !add_files then (
    if has_metadir then
      write_meta true !metadir meta_dot_pkg
    else
      write_meta false pkgdir "META";
  );

  (* Check if there is a postinstall script: *)
  let postinstall = Filename.concat !destdir "postinstall" in
  if Sys.file_exists postinstall then
    run_command Verbose postinstall [ slashify !destdir; !pkgname ]
;;


let reserved_names = [ Findlib_config.libexec_name; "postinstall"; "postremove" ];;

type fsmod_t =
    MOD_rm of string
  | MOD_rmdir of string
  | MOD_create of string * bytes
  | MOD_update of string * bytes
  | MOD_exec of string * string list
;;

let prepare_remove_package () =
  let destdir = ref (default_location()) in
  let destdir_set = ref false in
  let metadir = ref (meta_directory()) in
  let ldconf  = ref (ocaml_ldconf()) in
  let pkgname = ref "" in

  let keywords =
    [ "-destdir", (Arg.String (fun s -> destdir := s; destdir_set := true)),
              ("<path>      Set the destination directory (default: " ^
	       !destdir ^ ")");
      "-metadir", (Arg.String (fun s -> metadir := s)),
              ("<path>      Remove the META file from this directory (default: " ^
	       (if !metadir = "" then "none" else !metadir) ^ ")");
      "-ldconf", (Arg.String (fun s -> ldconf := s)),
             ("<path>       Update this ld.conf file (default: " ^ !ldconf ^ ")");
    ] in
  let errmsg = "usage: ocamlfind remove [options] <package_name>" in

  parse_args
        keywords
	(fun s ->
	   if !pkgname = ""
	   then pkgname := s
	   else raise (Arg.Bad "too many arguments")
	)
	errmsg;
  if !pkgname = "" then (Arg.usage keywords errmsg; exit 1);
  if List.mem !pkgname reserved_names then
    failwith ("You are not allowed to remove this thing by ocamlfind!");
  if not (Fl_split.is_valid_package_name !pkgname) then
    failwith "Package names must not contain the character '.'!";

  let meta_dot_pkg = "META." ^ !pkgname in
  let has_metadir = !metadir <> "" in
  let pkgdir = Filename.concat !destdir !pkgname in
  let dlldir = Filename.concat !destdir Findlib_config.libexec_name in
  let have_libexec = Sys.file_exists dlldir in

  (* Warn if there is another package with the same name: *)
  let other_pkgdir =
    try Findlib.package_directory !pkgname with No_such_package _ -> "" in
  if other_pkgdir <> "" && not !destdir_set then begin
    (* Is pkgdir = other_pkgdir? - We check physical identity: *)
    try
      let s_other_pkgdir = Unix.stat other_pkgdir in
      try
	let s_pkgdir = Unix.stat pkgdir in
	if (s_pkgdir.Unix.st_dev <> s_other_pkgdir.Unix.st_dev) ||
	   (s_pkgdir.Unix.st_ino <> s_other_pkgdir.Unix.st_ino)
	then
	  prerr_endline("ocamlfind: [WARNING] You are removing the package from " ^ pkgdir ^ " but the currently visible package is at " ^ other_pkgdir ^ "; you may want to specify the -destdir option");
      with
	  Unix.Unix_error(Unix.ENOENT,_,_) ->
	    prerr_endline("ocamlfind: [WARNING] You are trying to remove the package from " ^ pkgdir ^ " but the currently visible package is at " ^ other_pkgdir ^ "; you may want to specify the -destdir option");
    with
	Unix.Unix_error(_,_,_) -> ()    (* ignore, it's only a warning *)
  end;

  (* First remove the META file. If it is already gone, assume that a
     parallel running removal removed it already.
   *)

  let mods = ref [] in
  let push_rm s = mods := (MOD_rm s) :: !mods in
  let push_rmdir s = mods := (MOD_rmdir s) :: !mods in
  let push_update s bytes = mods := (MOD_update (s, bytes)) :: !mods in
  let push_exec cmd args = mods := (MOD_exec (cmd, args)) :: !mods in

  if has_metadir then push_rm (Filename.concat !metadir meta_dot_pkg)
  else push_rm (Filename.concat pkgdir "META") ;

  (* Remove files from libexec directory: *)
  if have_libexec then begin
    let dll_files = find_owned_files !pkgname dlldir in
    List.iter
      (fun file ->
         let absfile = Filename.concat dlldir file in
         push_rm absfile
      )
      dll_files
  end;

    (* Remove the files from the package directory: *)
    if Sys.file_exists pkgdir then begin
      let files = Sys.readdir pkgdir in
      Array.iter (fun f -> push_rm (Filename.concat pkgdir f)) files;
      push_rmdir pkgdir;
    end
    else
      prerr_endline("ocamlfind: [WARNING] No such directory: " ^ pkgdir);


  (* Modify ld.conf *)
  if !ldconf <> "ignore" && Sys.file_exists !ldconf then
    begin
      let lines = read_ldconf !ldconf in
      let d = Fl_split.norm_dir pkgdir in
      let exists = List.exists (fun p -> Fl_split.norm_dir p = d) lines in
      if exists then begin
        let lines' = List.filter (fun p -> Fl_split.norm_dir p <> d) lines in
        push_update !ldconf (format_ldconf lines' [])
      end
    end;

  (* Check if there is a postremove script: *)
  let postremove = Filename.concat !destdir "postremove" in
  if Sys.file_exists postremove then
    push_exec  postremove [ slashify !destdir; !pkgname ] ;

  List.rev !mods
;;

let passthru () =
  run_command Normal "ocamlfind" (List.tl (Array.to_list !argv))
;;


let rec select_mode () =
  let k = !current in
  let m_string = try arg (k+1) with Not_found -> raise Usage in
  let m =
    match m_string with
      ("reinstall-if-diff")               -> incr current; M_reinstall_if_diff
    | ("preprocess")             -> incr current; M_preprocess
    | _ -> M_passthru
  in

  m
;;


let _main av curr () =
  argv := av ; current := curr ;
  try
    let m = select_mode() in
    match m with
      M_reinstall_if_diff        -> reinstall_if_diff_package()
    | M_preprocess -> preprocess ()
    | M_passthru       -> passthru()
  with
   Failure f ->
      prerr_endline ("ocamlfind: " ^ f);
      exit 2
  | Sys_error f ->
      prerr_endline ("ocamlfind: " ^ f);
      exit 2
  | Findlib.No_such_package(pkg,info) ->
      prerr_endline ("ocamlfind: Package `" ^ pkg ^ "' not found" ^
		     (if info <> "" then " - " ^ info else ""));
      exit 2
  | Findlib.Package_loop pkg ->
      prerr_endline ("ocamlfind: Package `" ^ pkg ^ "' requires itself");
      exit 2
  | Silent_error ->
      exit 2
;;

let main ?(argv = Sys.argv) ?(current = !Arg.current) () =
  _main argv current () ;;
