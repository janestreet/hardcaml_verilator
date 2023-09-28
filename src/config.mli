open! Core

type t =
  { verilator_version : Verilator_version.t
  ; optimization_level : Optimization_level.t
  ; compilation_processes : Threads.t
  ; runtime_threads : Threads.t
  ; output_split : Output_split.t
  ; verbose : bool
  }
[@@deriving sexp_of]

(** Default verilator compilation settings with no parallelism *)
val default : t

(** Split C-files and functions into much smaller chunks. *)
val small_cfiles : t

(** Small C-files with compilation parallelism specified with
    [VERILATOR_PARALLEL_COMPILE=n] *)
val from_env : t

val flag : t Command.Param.t

(* / *)

val label : t -> string
val executable : t -> string

val verilator_compilation_command
  :  ?executable:string
  -> circuit_name:string
  -> path_to_verilog:string
  -> obj_dir:string
  -> path_to_cpp_wrapper:string
  -> t
  -> string

val make_compilation_command : circuit_name:string -> obj_dir:string -> t -> string

val final_link_command
  :  obj_dir:string
  -> path_to_shared_lib:string
  -> path_to_static_lib:string
  -> t
  -> string
