open Core

type t [@@deriving sexp_of]

val create : int -> t
val to_string : t -> string
val to_int : t -> int
val flag : string -> t Command.Param.t
