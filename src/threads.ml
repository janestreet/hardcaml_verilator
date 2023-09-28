open Core

type t = int [@@deriving sexp_of]

let create threads =
  if threads <= 0
  then raise_s [%message "number of threads must be one or more" (threads : int)];
  threads
;;

let to_int t = t
let to_string = Int.to_string

let flag name =
  [%map_open.Command
    let threads =
      flag name (optional_with_default 1 int) ~doc:"Number of parallel threads (>0)"
    in
    create threads]
;;
