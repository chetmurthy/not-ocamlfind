
module Pa_ppx_runtime =
  struct
    module Runtime = struct module Fmt = Fmt end
  end

type file_t =
  { name : string; checksum : string }[@@deriving_inline show, sexp]
let rec (sexp_of_file_t : file_t -> Sexplib.Sexp.t) =
  fun arg ->
    (let open! Pa_ppx_runtime.Runtime in
     let open! Stdlib in
     fun ({name = v_name; checksum = v_checksum} : file_t) ->
       Sexplib0.Sexp.List
         (let fields = [] in
          let fields =
            Sexplib0.Sexp.List
              [Sexplib0.Sexp.Atom "checksum";
               Sexplib0.Sexp_conv.sexp_of_string v_checksum] ::
            fields
          in
          let fields =
            Sexplib0.Sexp.List
              [Sexplib0.Sexp.Atom "name";
               Sexplib0.Sexp_conv.sexp_of_string v_name] ::
            fields
          in
          fields))
      arg[@@ocaml.warning "-39"] [@@ocaml.warning "-33"]
and (file_t_of_sexp : Sexplib.Sexp.t -> file_t) =
  fun arg ->
    (let open! Pa_ppx_runtime.Runtime in
     let open! Stdlib in
     function
       Sexplib0.Sexp.List xs ->
         let rec loop xs (v_name, v_checksum) =
           match xs with
             Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "name"; v_name] :: xs ->
               loop xs
                 (Result.Ok (Sexplib0.Sexp_conv.string_of_sexp v_name),
                  v_checksum)
           | Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "checksum"; v_checksum] ::
             xs ->
               loop xs
                 (v_name,
                  Result.Ok (Sexplib0.Sexp_conv.string_of_sexp v_checksum))
           | [] ->
               Rresult.R.bind v_name
                 (fun v_name ->
                    Rresult.R.bind v_checksum
                      (fun v_checksum ->
                         Result.Ok {name = v_name; checksum = v_checksum}))
           | _ :: xs -> loop xs (v_name, v_checksum)
         in
         begin match
           loop xs
             (Result.Error "Fsmod.file_t.name",
              Result.Error "Fsmod.file_t.checksum")
         with
           Result.Ok r -> r
         | Result.Error msg -> failwith msg
         end
     | _ -> failwith "Fsmod.file_t")
      arg[@@ocaml.warning "-39"] [@@ocaml.warning "-33"]
let rec (pp_file_t : file_t Fmt.t) =
  fun (ofmt : Format.formatter) arg ->
    (fun ofmt ({name = v_name; checksum = v_checksum} : file_t) ->
       let open Pa_ppx_runtime.Runtime.Fmt in
       pf ofmt "@[<2>{ @[Fsmod.name =@ %a@];@ @[checksum =@ %a@] }@]"
         (fun ofmt arg ->
            let open Pa_ppx_runtime.Runtime.Fmt in pf ofmt "%S" arg)
         v_name
         (fun ofmt arg ->
            let open Pa_ppx_runtime.Runtime.Fmt in pf ofmt "%S" arg)
         v_checksum)
      ofmt arg[@@ocaml.warning "-39"] [@@ocaml.warning "-33"]
and (show_file_t : file_t -> Stdlib.String.t) =
  fun arg -> Format.asprintf "%a" pp_file_t arg[@@ocaml.warning "-39"] [@@ocaml.warning "-33"]
[@@@end]
type t =
    FM_delete of file_t
  | FM_install of file_t
  | FM_update of file_t * string
  | FM_create of file_t * string[@@deriving_inline show, sexp]
let rec (sexp_of_t : t -> Sexplib.Sexp.t) =
  fun arg ->
    (let open! Pa_ppx_runtime.Runtime in
     let open! Stdlib in
     function
       FM_delete v0 ->
         Sexplib0.Sexp.List
           [Sexplib0.Sexp.Atom "FM_delete"; sexp_of_file_t v0]
     | FM_install v0 ->
         Sexplib0.Sexp.List
           [Sexplib0.Sexp.Atom "FM_install"; sexp_of_file_t v0]
     | FM_update (v0, v1) ->
         Sexplib0.Sexp.List
           [Sexplib0.Sexp.Atom "FM_update"; sexp_of_file_t v0;
            Sexplib0.Sexp_conv.sexp_of_string v1]
     | FM_create (v0, v1) ->
         Sexplib0.Sexp.List
           [Sexplib0.Sexp.Atom "FM_create"; sexp_of_file_t v0;
            Sexplib0.Sexp_conv.sexp_of_string v1])
      arg[@@ocaml.warning "-39"] [@@ocaml.warning "-33"]
and (t_of_sexp : Sexplib.Sexp.t -> t) =
  fun arg ->
    (let open! Pa_ppx_runtime.Runtime in
     let open! Stdlib in
     function
       Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "FM_delete"; v0] ->
         FM_delete (file_t_of_sexp v0)
     | Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "FM_install"; v0] ->
         FM_install (file_t_of_sexp v0)
     | Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "FM_update"; v0; v1] ->
         FM_update (file_t_of_sexp v0, Sexplib0.Sexp_conv.string_of_sexp v1)
     | Sexplib0.Sexp.List [Sexplib0.Sexp.Atom "FM_create"; v0; v1] ->
         FM_create (file_t_of_sexp v0, Sexplib0.Sexp_conv.string_of_sexp v1)
     | _ -> failwith "Fsmod.t")
      arg[@@ocaml.warning "-39"] [@@ocaml.warning "-33"]
let rec (pp : t Fmt.t) =
  fun (ofmt : Format.formatter) arg ->
    (fun ofmt ->
       function
         FM_delete v0 ->
           let open Pa_ppx_runtime.Runtime.Fmt in
           pf ofmt "(@[<2>Fsmod.FM_delete@ %a)@]" pp_file_t v0
       | FM_install v0 ->
           let open Pa_ppx_runtime.Runtime.Fmt in
           pf ofmt "(@[<2>Fsmod.FM_install@ %a)@]" pp_file_t v0
       | FM_update (v0, v1) ->
           let open Pa_ppx_runtime.Runtime.Fmt in
           pf ofmt "(@[<2>Fsmod.FM_update@ (@,%a,@ %a@,))@]" pp_file_t v0
             (fun ofmt arg ->
                let open Pa_ppx_runtime.Runtime.Fmt in pf ofmt "%S" arg)
             v1
       | FM_create (v0, v1) ->
           let open Pa_ppx_runtime.Runtime.Fmt in
           pf ofmt "(@[<2>Fsmod.FM_create@ (@,%a,@ %a@,))@]" pp_file_t v0
             (fun ofmt arg ->
                let open Pa_ppx_runtime.Runtime.Fmt in pf ofmt "%S" arg)
             v1)
      ofmt arg[@@ocaml.warning "-39"] [@@ocaml.warning "-33"]
and (show : t -> Stdlib.String.t) =
  fun arg -> Format.asprintf "%a" pp arg[@@ocaml.warning "-39"] [@@ocaml.warning "-33"]
[@@@end]

type t_pair = string * t[@@deriving_inline show, sexp]
let rec (sexp_of_t_pair : t_pair -> Sexplib.Sexp.t) =
  fun arg ->
    (let open! Pa_ppx_runtime.Runtime in
     let open! Stdlib in
     fun (v0, v1) ->
       Sexplib0.Sexp.List
         [Sexplib0.Sexp_conv.sexp_of_string v0; sexp_of_t v1])
      arg[@@ocaml.warning "-39"] [@@ocaml.warning "-33"]
and (t_pair_of_sexp : Sexplib.Sexp.t -> t_pair) =
  fun arg ->
    (let open! Pa_ppx_runtime.Runtime in
     let open! Stdlib in
     function
       Sexplib0.Sexp.List [v0; v1] ->
         Sexplib0.Sexp_conv.string_of_sexp v0, t_of_sexp v1
     | _ -> failwith "wrong number of members in list")
      arg[@@ocaml.warning "-39"] [@@ocaml.warning "-33"]
let rec (pp_t_pair : t_pair Fmt.t) =
  fun (ofmt : Format.formatter) arg ->
    (fun (ofmt : Format.formatter) (v0, v1) ->
       let open Pa_ppx_runtime.Runtime.Fmt in
       pf ofmt "(@[%a,@ %a@])"
         (fun ofmt arg ->
            let open Pa_ppx_runtime.Runtime.Fmt in pf ofmt "%S" arg)
         v0 pp v1)
      ofmt arg[@@ocaml.warning "-39"] [@@ocaml.warning "-33"]
and (show_t_pair : t_pair -> Stdlib.String.t) =
  fun arg -> Format.asprintf "%a" pp_t_pair arg[@@ocaml.warning "-39"] [@@ocaml.warning "-33"]
[@@@end]
type t_pair_list = t_pair list[@@deriving_inline show, sexp]
let rec (sexp_of_t_pair_list : t_pair_list -> Sexplib.Sexp.t) =
  fun arg ->
    (let open! Pa_ppx_runtime.Runtime in
     let open! Stdlib in Sexplib0.Sexp_conv.sexp_of_list sexp_of_t_pair)
      arg[@@ocaml.warning "-39"] [@@ocaml.warning "-33"]
and (t_pair_list_of_sexp : Sexplib.Sexp.t -> t_pair_list) =
  fun arg ->
    (let open! Pa_ppx_runtime.Runtime in
     let open! Stdlib in Sexplib0.Sexp_conv.list_of_sexp t_pair_of_sexp)
      arg[@@ocaml.warning "-39"] [@@ocaml.warning "-33"]
let rec (pp_t_pair_list : t_pair_list Fmt.t) =
  fun (ofmt : Format.formatter) arg ->
    (fun (ofmt : Format.formatter) arg ->
       let open Pa_ppx_runtime.Runtime.Fmt in
       pf ofmt "@[<2>[%a@,]@]" (list ~sep:semi pp_t_pair) arg)
      ofmt arg[@@ocaml.warning "-39"] [@@ocaml.warning "-33"]
and (show_t_pair_list : t_pair_list -> Stdlib.String.t) =
  fun arg -> Format.asprintf "%a" pp_t_pair_list arg[@@ocaml.warning "-39"] [@@ocaml.warning "-33"]
[@@@end]
