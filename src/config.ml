open! Core

type t =
  { optimization_level : Optimization_level.t
  ; compilation_processes : Threads.t
  ; runtime_threads : Threads.t
  ; output_split : Output_split.t
  ; verbose : bool
  ; compilation_stats : bool
  }
[@@deriving sexp_of]

let default =
  { optimization_level = O3
  ; compilation_processes = Threads.create 1
  ; runtime_threads = Threads.create 1
  ; output_split = Output_split.create ()
  ; verbose = false
  ; compilation_stats = false
  }
;;

let small_cfiles =
  { optimization_level = O3
  ; compilation_processes = Threads.create 1
  ; runtime_threads = Threads.create 1
  ; output_split = Output_split.create ~lines_per_file:5000 ~lines_per_function:500 ()
  ; verbose = false
  ; compilation_stats = false
  }
;;

let from_env =
  match Sys.getenv "VERILATOR_PARALLEL_COMPILE" with
  | None -> default
  | Some processes ->
    { small_cfiles with compilation_processes = Threads.create (Int.of_string processes) }
;;

let label t =
  String.concat
    ~sep:"-"
    [ Optimization_level.to_string t.optimization_level
    ; "procs" ^ Threads.to_string t.compilation_processes
    ; "threads" ^ Threads.to_string t.runtime_threads
    ; Output_split.to_string t.output_split
    ]
;;

let executable _t = Hardcaml.Tools_config.verilator

let flag =
  [%map_open.Command
    let optimization_level = Optimization_level.flag
    and compilation_processes = Threads.flag "-compilation-processes"
    and runtime_threads = Threads.flag "-runtime-threads"
    and output_split = Output_split.flag
    and verbose =
      flag "-verilator-verbose" no_arg ~doc:"Show verilator compilation commands"
    and compilation_stats =
      flag "-compilation-stats" no_arg ~doc:"Show verilator compilation stats"
    in
    { optimization_level
    ; compilation_processes
    ; runtime_threads
    ; output_split
    ; verbose
    ; compilation_stats
    }]
;;

let needs_threaded_verilator_runtime _t = true

let runtime_thread_option t =
  "--threads " ^ Int.to_string (Threads.to_int t.runtime_threads)
;;

let optimization_flag t = Optimization_level.optimization_option t.optimization_level

let verilator_flags t ~circuit_name =
  String.concat
    ~sep:" "
    [ optimization_flag t
    ; runtime_thread_option t
    ; "--top-module " ^ circuit_name
    ; Output_split.split_options t.output_split
    ; (if t.compilation_stats then "--stats" else "")
    ]
;;

type make_flags =
  { make_envvars : string
  ; make_jobs : string
  }

let make_flags t =
  let make_envvars, make_jobs =
    match Threads.to_int t.compilation_processes with
    | 1 -> "", ""
    | jobs -> "VM_PARALLEL_BUILDS=1", sprintf "-j%i" jobs
  in
  { make_envvars; make_jobs }
;;

let relevant_object_files t =
  let base = [ "wrapper.o"; "verilated.o" ] in
  base @ if needs_threaded_verilator_runtime t then [ "verilated_threads.o" ] else []
;;

let verilator_compilation_command
  ?executable:exe
  ~circuit_name
  ~path_to_verilog
  ~obj_dir
  ~path_to_cpp_wrapper
  t
  =
  let verilator_flags = verilator_flags t ~circuit_name in
  sprintf
    "CXXFLAGS=\"-fPIC\" %s %s -Wno-COMBDLY -Wno-CMPCONST -Wno-UNSIGNED -Wno-INITIALDLY \
     -Wno-MULTIDRIVEN --cc %s  --Mdir %s %s"
    (Option.value ~default:(executable t) exe)
    verilator_flags
    path_to_verilog
    obj_dir
    path_to_cpp_wrapper
;;

let make_compilation_command ~circuit_name ~obj_dir t =
  let { make_envvars; make_jobs } = make_flags t in
  (* OBJCACHE controls the use of ccache. If it is not present, then by default ccache
     gets used, which we dont want. The semantics here are to use OBJCACHE if set,
     otherwise set it to nothing (but actually set it) so no caching is performed. *)
  let objcache = Option.value ~default:"" (Sys.getenv "OBJCACHE") in
  sprintf
    "CXXFLAGS=\"-fPIC -g %s\" OBJCACHE=\"%s\" %s make %s -C %s -f V%s.mk V%s__ALL.a %s"
    (optimization_flag t)
    objcache
    make_envvars
    make_jobs
    obj_dir
    circuit_name
    circuit_name
    (String.concat ~sep:" " (relevant_object_files t))
;;

let final_link_command ~obj_dir ~path_to_shared_lib ~path_to_static_lib t =
  sprintf
    "g++ -latomic -lpthread -fPIC %s -g -shared -o %s %s %s"
    (optimization_flag t)
    path_to_shared_lib
    (String.concat
       ~sep:" "
       (List.map ~f:(fun a -> obj_dir ^/ a) (relevant_object_files t)))
    path_to_static_lib
;;
