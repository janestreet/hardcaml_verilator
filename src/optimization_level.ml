open! Core

type t =
  | O0
  | O1
  | O2
  | O3
[@@deriving sexp_of]

let to_string t = Sexp.to_string (sexp_of_t t)

let flag =
  [%map_open.Command
    let opt =
      flag "-O" (optional_with_default 3 int) ~doc:"Compilation optimization level"
    in
    match opt with
    | 0 -> O0
    | 1 -> O1
    | 2 -> O2
    | 3 -> O3
    | _ -> raise_s [%message "Invalid optimization level must be [1..3]" (opt : int)]]
;;

let optimization_option t = "-" ^ to_string t
