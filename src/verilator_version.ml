open! Core

type t =
  | V4
  | V5
[@@deriving sexp_of]

let to_string t = Sexp.to_string (sexp_of_t t)

let of_int version =
  match version with
  | 4 -> V4
  | 5 -> V5
  | _ -> raise_s [%message "Invalid verilator version" (version : int)]
;;

let flag =
  [%map_open.Command
    let version =
      flag "-V" (optional_with_default Setup.default_version int) ~doc:"Verilator version"
    in
    of_int version]
;;
