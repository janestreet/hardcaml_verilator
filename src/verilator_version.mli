open! Core

type t =
  | V4
  | V5
[@@deriving sexp_of]

val to_string : t -> string
val of_int : int -> t
val flag : t Command.Param.t
