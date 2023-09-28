open! Core

type t =
  | O0
  | O1
  | O2
  | O3
[@@deriving sexp_of]

val to_string : t -> string
val flag : t Command.Param.t

(* / *)

val optimization_option : t -> string
