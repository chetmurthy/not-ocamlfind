
try
  Sys.catch_break true;
  Frontend.main()
with
  any ->
    prerr_endline ("Uncaught exception: " ^ Printexc.to_string any);
    let raise_again =
      try ignore(Sys.getenv "OCAMLFIND_DEBUG"); true
      with Not_found -> false
    in
    if raise_again then raise any;
    exit 3
;;
