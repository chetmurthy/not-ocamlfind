
module Pa_ppx_runtime = struct
  module Runtime = struct
    module Fmt = Fmt
  end
end

type file_t = { name : string ; checksum : string }[@@deriving show]
type t =
    FM_delete of file_t
  | FM_install of file_t
  | FM_update of file_t * string
  | FM_create of file_t * string[@@deriving show]

type t_pair = string * t[@@deriving show]
type t_pair_list = t_pair list[@@deriving show]
