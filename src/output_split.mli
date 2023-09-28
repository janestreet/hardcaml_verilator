open! Core

type t [@@deriving sexp_of]

val create : ?lines_per_file:int -> ?lines_per_function:int -> unit -> t
val to_string : t -> string
val flag : t Command.Param.t

(* / *)

val split_options : t -> string
val v4_compat : t -> t
