(** {v
 Verilator backend for Hardcaml cycle simulations.

    While this performs faster than Hardcaml's Cyclesim, it takes a longer time to
    elaborate the design (a ~3_000 LoC verilog file takes around 2 seconds, whilist
    hardcaml's Cyclesim takes 0.1 second). The default is to run it in non-thread safe
    mode (ie: single threaded w/o atomics / locks), which is the preferred option
    for small designs. The [create] and [With_interface.create] functions do the
    following:

    0. Generates the verilog file of the circuit
    1. Calls verilator to generate C++ file for simulation
    3. Generates C-wrapper for accessing fields in the C++ data structure
    4. Compiles the generated C++ file and C-wrapper to make a shared library
    5. Dyanmically load the shared library back to the same executable and bind the
    >    functions using C-types
    6. Create a [Cyclesim.t] instance by supplying the relevant functions with bindings to
    >    verilator
    v} *)

open Core
open Hardcaml
module Optimization_level = Optimization_level
module Threads = Threads
module Output_split = Output_split
module Verilator_version = Verilator_version
module Config = Config

module Cache : sig
  type t =
    | No_cache
    | Hashed of
        { cache_dir : string
        ; raise_if_not_found : bool
        (** Raises if the shared object cannot be found in the [cache_dir] provided,
            otherwise a new one will be compiled. *)
        }
    | Explicit of { file_name : string }
  [@@deriving sexp, bin_io]
end

module Simulation_backend : sig
  type t =
    | Hardcaml
    | Verilator of Cache.t
  [@@deriving sexp, bin_io]

  val flag : t Core.Command.Param.t
end

type input_port = Bits.t -> unit
type output_port = unit -> Bits.t
type internal_getter

type internal_port =
  { signal : Signal.t
  ; getter : internal_getter
  ; aliases : string list
  }

type t =
  { input_setters : (string, input_port) List.Assoc.t
  ; output_getters : (string, output_port) List.Assoc.t
  ; internal_getters : (string, internal_port) List.Assoc.t
  ; eval : unit -> unit
  ; complete : unit -> unit
  }

(** Arguments when creating a verilator simulation object.

    - [cache] specifies whether and where to store compiled shared libraries. When set to
      a directory, the [create] functions below first tries to check if an existing
      compilation for the current circuit exists in the specified directory. When set to a
      file, the file is used directly as the shared library. This can speed up compilation
      for repeated simulation runs.
    - [build_dir] specifies the build directory. Defaults to somewhere in /tmp.
    - [optimizations] specifies whether verilator optimizations should be turned on.
    - [parallel_compile] specifies whether the verilator simulation object should be
      compiled in parallel, and if so with how many parallel jobs.
    - [threads] speficies whether the verilator simulation object should be generated to
      be run in parallel, and if so with how many parallel threads.
    - [version] specifies which major version of verilator we want to use. *)
type 'a with_options =
  ?cache:Cache.t
  -> ?build_dir:string
  -> ?verilator_config:Config.t
  -> ?config:Cyclesim.Config.t
  -> 'a

val compile_circuit_and_load_shared_object : (Circuit.t -> t) with_options
val create : (clock_names:string list -> Circuit.t -> Cyclesim.t_port_list) with_options

module With_interface (I : Hardcaml.Interface.S) (O : Hardcaml.Interface.S) : sig
  val create
    : (clock_names:string list
       -> (Signal.t I.t -> Signal.t O.t)
       -> (Bits.t ref I.t, Bits.t ref O.t) Cyclesim.t)
        with_options

  (** Compiles a circuit and returns the shared object which can later be passed as the
      [file_name] with Cache.Explicit to the [cache] input. *)
  val create_shared_object : ((Signal.t I.t -> Signal.t O.t) -> string) with_options
end
