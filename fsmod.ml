
module Pa_ppx_runtime =
  struct
    module Runtime = struct module Fmt = Fmt end
  end

type file_t = { name : string; checksum : string }[@@deriving_inline show]

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
  | FM_create of file_t * string[@@deriving_inline show]
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

type t_pair = string * t[@@deriving_inline show]

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
type t_pair_list = t_pair list[@@deriving_inline show]
let rec (pp_t_pair_list : t_pair_list Fmt.t) =
  fun (ofmt : Format.formatter) arg ->
    (fun (ofmt : Format.formatter) arg ->
       let open Pa_ppx_runtime.Runtime.Fmt in
       pf ofmt "@[<2>[%a@,]@]" (list ~sep:semi pp_t_pair) arg)
      ofmt arg[@@ocaml.warning "-39"] [@@ocaml.warning "-33"]
and (show_t_pair_list : t_pair_list -> Stdlib.String.t) =
  fun arg -> Format.asprintf "%a" pp_t_pair_list arg[@@ocaml.warning "-39"] [@@ocaml.warning "-33"]
[@@@end]
