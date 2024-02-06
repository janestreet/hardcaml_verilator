open Core
open Hardcaml
open Ctypes
open Ctypes_foreign_flat
open Foreign
module Unix = Core_unix
module Optimization_level = Optimization_level
module Threads = Threads
module Output_split = Output_split
module Verilator_version = Verilator_version
module Config = Config

module Cache = struct
  type t =
    | No_cache
    | Hashed of { cache_dir : string }
    | Explicit of { file_name : string }
  [@@deriving sexp, bin_io]
end

module Simulation_backend = struct
  type t =
    | Hardcaml
    | Verilator of Cache.t
  [@@deriving sexp, bin_io]

  let flag =
    let open Core.Command.Let_syntax in
    [%map_open
      let use_verilator = flag "verilator" no_arg ~doc:"<flag> Use the verilator backend"
      and verilator_cache_dir =
        flag
          "verilator-cache-dir"
          (optional string)
          ~doc:
            "<directory> Cache directory for verilator. This can only be set when with \
             the -verilator flag.)"
      and file_name =
        flag
          "verilator-file-name"
          (optional string)
          ~doc:
            "<filename> Explicit file name to use for shared library.  This can only be \
             set with the -verilator flag"
      in
      if use_verilator
      then (
        match verilator_cache_dir, file_name with
        | None, None -> Verilator No_cache
        | Some cache_dir, None -> Verilator (Hashed { cache_dir })
        | None, Some file_name -> Verilator (Explicit { file_name })
        | Some _, Some _ ->
          raise_s
            [%message
              "only one of -verilator-cache-dir and -verilator-file-name may be specified"])
      else (
        match verilator_cache_dir, file_name with
        | None, None -> Hardcaml
        | _ ->
          raise_s
            [%message
              "-verilator-cache-dir and -verilator-file-name may only be specified along \
               with -verilator flag"])]
  ;;
end

let verilator_t : [ `verilator ] structure typ = structure "verilator"

type input_port = Bits.t -> unit
type output_port = unit -> Bits.t

type internal_port =
  { signal : Signal.t
  ; bits : Bits.Mutable.t
  ; update : unit -> unit
  ; aliases : string list
  }

type t =
  { input_setters : (string, input_port) List.Assoc.t
  ; output_getters : (string, output_port) List.Assoc.t
  ; internal_getters : (string, internal_port) List.Assoc.t
  ; eval : unit -> unit
  ; complete : unit -> unit
  }

type 'a with_options =
  ?cache:Cache.t
  -> ?build_dir:string
  -> ?verilator_config:Config.t
  -> ?config:Cyclesim.Config.t
  -> 'a

let sprintf = Printf.sprintf
let bprintf = Printf.bprintf
let init_name ~circuit_name = sprintf "hardcaml_verilator_%s_init" circuit_name
let complete_name ~circuit_name = sprintf "hardcaml_verilator_%s_final" circuit_name
let eval_name ~circuit_name = sprintf "hardcaml_verilator_%s_eval" circuit_name

(* This is obtained based on an empirical observation of verilator generated outputs. *)
let sanitize_port_name port_name =
  port_name
  |> String.substr_replace_all ~pattern:"__" ~with_:"___05F"
  |> String.substr_replace_all ~pattern:"$" ~with_:"__024"
;;

let input_addr_fn_name ~circuit_name ~port_name =
  sprintf "hardcaml_verilator_%s_%s_addr" circuit_name (sanitize_port_name port_name)
;;

let output_addr_fn_name ~circuit_name ~port_name =
  sprintf "hardcaml_verilator_%s_%s_addr" circuit_name (sanitize_port_name port_name)
;;

let internal_signals
  ?(verilog_name_by_id : Rtl.Signals_name_map.t option)
  circuit
  (config : Cyclesim.Config.t)
  =
  match config.is_internal_port with
  | None -> []
  | Some f ->
    let is_internal s =
      (not (Circuit.is_input circuit s))
      && (not (Circuit.is_output circuit s))
      && (not (Signal.is_empty s))
      && f s
    in
    Signal_graph.filter (Circuit.signal_graph circuit) ~f:is_internal
    |> List.filter_map ~f:(fun signal ->
         match Signal.names signal with
         | [] -> None (* only use named internal signals *)
         | names ->
           let uid = Signal.uid signal in
           let names =
             match verilog_name_by_id with
             | None -> names
             | Some verilog_name_by_id ->
               List.mapi names ~f:(fun idx _ ->
                 let lookup = uid, idx in
                 Map.find_exn verilog_name_by_id lookup)
           in
           Some (signal, names))
;;

let[@inline always] ceil_div a b = if a % b = 0 then a / b else (a / b) + 1

(* If verbose is set, the output will be printed anyway, otherwise capture the output and
   display it on error. *)
let with_tmp_stdout ~verbose ~f =
  let tmp_stdout =
    if verbose then None else Some (Filename_unix.temp_file "verilator_run" ".stdout")
  in
  f tmp_stdout;
  Option.iter tmp_stdout ~f:Unix.unlink
;;

let run_command_exn ?(verbose = false) command =
  with_tmp_stdout ~verbose ~f:(fun tmp_stdout ->
    let command =
      match tmp_stdout with
      | Some tmp_stdout -> [%string "%{command} &>%{tmp_stdout}"]
      | None -> command
    in
    match Unix.system command with
    | Ok () -> ()
    | Error e ->
      let stdout = Option.map tmp_stdout ~f:In_channel.read_all in
      raise_s
        [%message
          (command : string) (e : Unix.Exit_or_signal.error) (stdout : string option)])
;;

let generate_cpp_wrapper
  buffer
  (circuit : Circuit.t)
  (config : Cyclesim.Config.t)
  (verilog_name_by_id : Rtl.Signals_name_map.t)
  =
  let circuit_name = Circuit.name circuit in
  let typ = "V" ^ circuit_name in
  bprintf buffer "#include <stdint.h>\n";
  bprintf buffer "#include \"verilated.h\"\n";
  bprintf buffer "#include \"V%s.h\"\n" circuit_name;
  let internal_signals = internal_signals circuit config ~verilog_name_by_id in
  if List.length internal_signals > 0
  then bprintf buffer "#include \"V%s_%s.h\"\n" circuit_name circuit_name;
  bprintf buffer "extern \"C\"{\n\n";
  let () =
    bprintf buffer "%s* %s(){\n" typ (init_name ~circuit_name);
    bprintf buffer "  return new %s;\n" typ;
    bprintf buffer "};\n"
  in
  let () =
    bprintf buffer "void %s(%s * p){\n" (eval_name ~circuit_name) typ;
    bprintf buffer "  p->eval();\n";
    bprintf buffer "};\n"
  in
  let () =
    bprintf buffer "void %s(%s * p){\n" (complete_name ~circuit_name) typ;
    bprintf buffer "  p->final();\n";
    bprintf buffer "  delete p;\n";
    bprintf buffer "};\n"
  in
  List.iter (Circuit.inputs circuit) ~f:(fun input ->
    let port_name = List.hd_exn (Signal.names input) in
    bprintf
      buffer
      "char* %s(%s * ptr) {\n"
      (input_addr_fn_name ~circuit_name ~port_name)
      typ;
    bprintf buffer "  return (char*) &(ptr->%s);\n" (sanitize_port_name port_name);
    bprintf buffer "}\n");
  List.iter (Circuit.outputs circuit) ~f:(fun output ->
    let port_name = List.hd_exn (Signal.names output) in
    bprintf
      buffer
      "const char* %s(%s * ptr) {\n"
      (output_addr_fn_name ~circuit_name ~port_name)
      typ;
    bprintf buffer "  return (char*) &(ptr->%s);\n" (sanitize_port_name port_name);
    bprintf buffer "}\n");
  List.iter internal_signals ~f:(fun (_, port_names) ->
    (* Only need to have a getter for one alias *)
    let port_name = List.hd_exn port_names in
    bprintf
      buffer
      "const char* %s(%s * ptr) {\n"
      (output_addr_fn_name ~circuit_name ~port_name)
      typ;
    bprintf
      buffer
      "  return (char*) &(ptr->%s->%s);\n"
      circuit_name
      (sanitize_port_name port_name);
    bprintf buffer "}\n");
  bprintf buffer "\n}\n"
;;

external caml_bytes_set16 : Bytes.t -> int -> int -> unit = "%caml_bytes_set16u"
external caml_bytes_set32 : Bytes.t -> int -> int32 -> unit = "%caml_bytes_set32u"
external caml_bytes_set64 : Bytes.t -> int -> int64 -> unit = "%caml_bytes_set64u"
external caml_bytes_get16 : Bytes.t -> int -> int = "%caml_bytes_get16u"
external caml_bytes_get32 : Bytes.t -> int -> int32 = "%caml_bytes_get32u"
external caml_bytes_get64 : Bytes.t -> int -> int64 = "%caml_bytes_get64u"

external caml_bigstring_set8
  :  Bigstring.t
  -> int
  -> char
  -> unit
  = "%caml_ba_unsafe_set_1"

external caml_bigstring_set16
  :  Bigstring.t
  -> int
  -> int
  -> unit
  = "%caml_bigstring_set16u"

external caml_bigstring_set32
  :  Bigstring.t
  -> int
  -> int32
  -> unit
  = "%caml_bigstring_set32u"

external caml_bigstring_set64
  :  Bigstring.t
  -> int
  -> int64
  -> unit
  = "%caml_bigstring_set64u"

external caml_bigstring_get8 : Bigstring.t -> int -> char = "%caml_ba_unsafe_ref_1"
external caml_bigstring_get16 : Bigstring.t -> int -> int = "%caml_bigstring_get16u"
external caml_bigstring_get32 : Bigstring.t -> int -> int32 = "%caml_bigstring_get32u"
external caml_bigstring_get64 : Bigstring.t -> int -> int64 = "%caml_bigstring_get64u"

let bytes_offset_for_data = Bits.Expert.offset_for_data

let copy_from_bytes_to_bigstring ~bit_width ~dst =
  if bit_width <= 8
  then fun src -> caml_bigstring_set8 dst 0 (Bytes.unsafe_get src bytes_offset_for_data)
  else if bit_width <= 16
  then fun src -> caml_bigstring_set16 dst 0 (caml_bytes_get16 src bytes_offset_for_data)
  else if bit_width <= 32
  then fun src -> caml_bigstring_set32 dst 0 (caml_bytes_get32 src bytes_offset_for_data)
  else if bit_width <= 64
  then fun src -> caml_bigstring_set64 dst 0 (caml_bytes_get64 src bytes_offset_for_data)
  else (
    let num_bytes = ceil_div bit_width 8 in
    fun src ->
      Bigstring.From_bytes.unsafe_blit
        ~src
        ~src_pos:bytes_offset_for_data
        ~dst
        ~dst_pos:0
        ~len:num_bytes)
;;

let copy_to_bytes_from_bigstring ~bit_width ~src =
  if bit_width <= 8
  then fun dst -> Bytes.unsafe_set dst bytes_offset_for_data (caml_bigstring_get8 src 0)
  else if bit_width <= 16
  then fun dst -> caml_bytes_set16 dst bytes_offset_for_data (caml_bigstring_get16 src 0)
  else if bit_width <= 32
  then fun dst -> caml_bytes_set32 dst bytes_offset_for_data (caml_bigstring_get32 src 0)
  else if bit_width <= 64
  then fun dst -> caml_bytes_set64 dst bytes_offset_for_data (caml_bigstring_get64 src 0)
  else (
    let num_bytes = ceil_div bit_width 8 in
    fun dst ->
      Bigstring.To_bytes.unsafe_blit
        ~src
        ~src_pos:0
        ~dst
        ~dst_pos:bytes_offset_for_data
        ~len:num_bytes)
;;

(* When converting bit widths from memory we need to round up each memory slot's bit_width
   to read correctly.

   Signals are the smallest of 8-bit unsigned chars (equivalent to uint8_t), 16-bit
   unsigned shorts (uint16_t), 32-bit unsigned longs (uint32_t), or 64-bit unsigned long
   longs (uint64_t) that fits the width of the signal. Signals wider than 64 bits are
   stored as an array of 32-bit uint32_t’s.
*)
let round_up_size i =
  if i <= 8
  then Int.round_up i ~to_multiple_of:8
  else if i <= 16
  then Int.round_up i ~to_multiple_of:16
  else Int.round_up i ~to_multiple_of:32
;;

let create_foreign_bindings
  ?from
  (circuit : Circuit.t)
  (internal_signals : (Signal.t * string list) list)
  =
  let circuit_name = Circuit.name circuit in
  let verilator_ptr =
    foreign ?from (init_name ~circuit_name) (void @-> returning (ptr verilator_t)) ()
  in
  let input_setters =
    List.map (Circuit.inputs circuit) ~f:(fun input ->
      let port_name = List.hd_exn (Signal.names input) in
      let foreign_address =
        foreign
          ?from
          (input_addr_fn_name ~circuit_name ~port_name)
          (ptr verilator_t @-> returning (ptr char))
          verilator_ptr
      in
      let bit_width = Signal.width input in
      let size = ceil_div bit_width 8 in
      let copy_into_verilator =
        copy_from_bytes_to_bigstring
          ~bit_width
          ~dst:(Ctypes.bigarray_of_ptr Ctypes.array1 size Bigarray.Char foreign_address)
      in
      ( port_name
      , fun bits ->
          if Bits.width bits <> Signal.width input
          then
            raise_s
              [%message
                "input port width mismatch!"
                  (port_name : string)
                  ~port_width:(Signal.width input : int)
                  ~value_width:(Bits.width bits : int)];
          copy_into_verilator (Bits.Expert.unsafe_underlying_repr bits) ))
  in
  let init_getters bit_width ~port_name =
    let size = ceil_div bit_width 8 in
    let foreign_address =
      foreign
        ?from
        (output_addr_fn_name ~circuit_name ~port_name)
        (ptr verilator_t @-> returning (ptr char))
        verilator_ptr
    in
    let copy_from_verilator =
      copy_to_bytes_from_bigstring
        ~bit_width
        ~src:(Ctypes.bigarray_of_ptr Ctypes.array1 size Bigarray.Char foreign_address)
    in
    port_name, copy_from_verilator
  in
  let bits_getters bit_width ~port_name =
    let name, copy_from_verilator = init_getters bit_width ~port_name in
    ( name
    , fun () ->
        let bits = Bits.of_int ~width:bit_width 0 in
        copy_from_verilator (Bits.Expert.unsafe_underlying_repr bits);
        bits )
  in
  let bits_mutable_getters signal bit_width ~port_name ~aliases =
    let name, copy_from_verilator = init_getters bit_width ~port_name in
    let bits = Bits.Mutable.create bit_width in
    ( name
    , { signal
      ; bits
      ; update = (fun () -> copy_from_verilator (bits :> Bytes.t))
      ; aliases
      } )
  in
  let output_getters =
    List.map (Circuit.outputs circuit) ~f:(fun output ->
      let port_name = List.hd_exn (Signal.names output) in
      let bit_width = Signal.width output in
      bits_getters bit_width ~port_name)
  in
  let internal_getters =
    let signals_and_names = internal_signals in
    List.map signals_and_names ~f:(fun (signal, port_names) ->
      (* Only use the getter for the first alias *)
      let port_name = List.hd_exn port_names in
      let bit_width =
        match signal with
        | Multiport_mem { size; _ } ->
          (* Memories are exposed as 2d arrays so adjust the size to make sure we round
             up and correctly read the entire contents. *)
          size * round_up_size (Signal.width signal)
        | _ -> Signal.width signal
      in
      bits_mutable_getters signal bit_width ~port_name ~aliases:port_names)
  in
  let complete =
    let f =
      foreign ?from (complete_name ~circuit_name) (ptr verilator_t @-> returning void)
    in
    fun () -> f verilator_ptr
  in
  let eval =
    let f =
      foreign ?from (eval_name ~circuit_name) (ptr verilator_t @-> returning void)
    in
    fun () -> f verilator_ptr
  in
  { eval; input_setters; output_getters; internal_getters; complete }
;;

let create_foreign_bindings_from_dllib circuit path_to_shared_lib internal_signals =
  let filename =
    (* dlopen requires an explicit path name to an existing file. *)
    match Filename_unix.realpath path_to_shared_lib with
    | path -> path
    | exception _ ->
      raise_s
        [%message "Could not find verilator shared library" (path_to_shared_lib : string)]
  in
  let dllib = Dl.dlopen ~filename ~flags:[ RTLD_NOW ] in
  create_foreign_bindings ~from:dllib circuit internal_signals
;;

let compile_circuit
  ?build_dir
  ~(verilator_config : Config.t)
  ~config
  ~verilog_contents
  ~circuit
  verilog_name_by_id
  ()
  =
  let ( ^/ ) = Stdlib.Filename.concat in
  let circuit_name = Circuit.name circuit in
  let build_dir =
    match build_dir with
    | None -> Filename_unix.temp_dir circuit_name ""
    | Some x -> x
  in
  let obj_dir = build_dir ^/ "obj_dir" in
  let path_to_cpp_wrapper =
    let filename = build_dir ^/ "wrapper.cpp" in
    let buffer = Buffer.create 1048 in
    generate_cpp_wrapper buffer circuit config verilog_name_by_id;
    Stdio.Out_channel.write_all filename ~data:(Buffer.contents buffer);
    filename
  in
  let path_to_verilog =
    let filename = build_dir ^/ Circuit.name circuit ^ ".v" in
    let oc = Out_channel.create filename in
    Out_channel.output_buffer oc verilog_contents;
    Out_channel.close oc;
    filename
  in
  let path_to_static_lib = obj_dir ^/ sprintf "V%s__ALL.a" (Circuit.name circuit) in
  let path_to_shared_lib = obj_dir ^/ sprintf "V%s__ALL.so" (Circuit.name circuit) in
  let verbose = verilator_config.verbose in
  let circuit_name = Circuit.name circuit in
  run_command_exn
    ~verbose
    (Config.verilator_compilation_command
       verilator_config
       ~circuit_name
       ~path_to_verilog
       ~obj_dir
       ~path_to_cpp_wrapper);
  run_command_exn
    ~verbose
    (Config.make_compilation_command verilator_config ~circuit_name ~obj_dir);
  run_command_exn
    ~verbose
    (Config.final_link_command
       verilator_config
       ~obj_dir
       ~path_to_shared_lib
       ~path_to_static_lib);
  path_to_shared_lib
;;

let compile_circuit_with_cache
  ?(cache = Cache.No_cache)
  ?build_dir
  ?(verilator_config = Config.from_env)
  ~config
  circuit
  =
  let shared_lib, verilog_name_by_id =
    let verilog_contents, verilog_name_by_id =
      let buffer = Buffer.create 32 in
      (* annotate all the signals that need to be exposed *)
      internal_signals circuit config
      |> List.iter ~f:(fun (s, _) ->
           ignore (Signal.set_comment s "verilator public" : Signal.t));
      let nm =
        Rtl.Expert.output_with_name_map
          ~output_mode:(Rtl.Output_mode.To_buffer buffer)
          Verilog
          circuit
      in
      buffer, nm
    in
    let compile_circuit () =
      compile_circuit
        ?build_dir
        ~verilator_config
        ~config
        ~circuit
        ~verilog_contents
        verilog_name_by_id
        ()
    in
    let options =
      let suffix = Config.label verilator_config in
      suffix ^ ".so"
    in
    let verbose = verilator_config.verbose in
    let check_cached_and_compile fname =
      if Sys_unix.file_exists_exn fname
      then fname
      else (
        let compiled = compile_circuit () in
        run_command_exn ~verbose (sprintf "cp %s %s" compiled fname);
        fname)
    in
    ( (match cache with
       | No_cache -> compile_circuit ()
       | Hashed { cache_dir } ->
         let md5 = Md5.digest_bytes (Buffer.contents_bytes verilog_contents) in
         run_command_exn ~verbose (sprintf "mkdir -p %s" cache_dir);
         let fname = cache_dir ^/ Md5_lib.to_hex md5 ^ "-" ^ options in
         check_cached_and_compile fname
       | Explicit { file_name } -> check_cached_and_compile file_name)
    , verilog_name_by_id )
  in
  shared_lib, internal_signals ~verilog_name_by_id circuit config
;;

let compile_circuit_and_load_shared_object
  ?cache
  ?build_dir
  ?verilator_config
  ?(config = Cyclesim.Config.default)
  circuit
  =
  let shared_lib, internal_signals =
    compile_circuit_with_cache ?cache ?build_dir ?verilator_config ~config circuit
  in
  create_foreign_bindings_from_dllib circuit shared_lib internal_signals
;;

let create
  ?cache
  ?build_dir
  ?verilator_config
  ?(config = Cyclesim.Config.default)
  ~clock_names
  circuit
  =
  let shared_object, (internal_signals : (Signal.t * string list) List.t) =
    compile_circuit_with_cache ?cache ?build_dir ?verilator_config ~config circuit
  in
  let make_port_list signals =
    List.map signals ~f:(fun s ->
      List.hd_exn (Signal.names s), ref (Bits.of_int ~width:(Signal.width s) 0))
  in
  let in_ports = make_port_list (Circuit.inputs circuit) in
  let internal_memories =
    List.filter_map internal_signals ~f:(fun (s, names) ->
      match s with
      | Multiport_mem { size; _ } ->
        Some
          (List.map names ~f:(fun n ->
             n, s, Array.init size ~f:(fun _ -> Bits.of_int ~width:(Signal.width s) 0)))
      | _ -> None)
    |> List.concat
  in
  let out_ports_before_clock_edge = make_port_list (Circuit.outputs circuit) in
  let out_ports_after_clock_edge = make_port_list (Circuit.outputs circuit) in
  let handle =
    create_foreign_bindings_from_dllib circuit shared_object internal_signals
  in
  let make_read_memories memory_ports getters : (unit -> unit) list =
    List.map memory_ports ~f:(fun (name, _, value_array) ->
      let { signal = _; bits; update = fn; aliases = _ } =
        getters |> List.find_exn ~f:(fun a -> String.equal (fst a) name) |> snd
      in
      fun () ->
        fn ();
        let value = Bits.Mutable.to_bits bits in
        let width = round_up_size (Bits.width value_array.(0)) in
        for i = 0 to Array.length value_array - 1 do
          value_array.(i) <- Bits.select value ((width * (i + 1)) - 1) (width * i)
        done)
  in
  let read_memories = make_read_memories internal_memories handle.internal_getters in
  let make_cycle_functions () =
    let make_read_outputs out_ports getters =
      List.map out_ports ~f:(fun (name, value_ref) ->
        let fn =
          getters |> List.find_exn ~f:(fun a -> String.equal (fst a) name) |> snd
        in
        fun () -> value_ref := fn ())
    in
    let make_read_internals getters =
      List.filter_map getters ~f:(fun (_, { signal; bits = _; update; aliases = _ }) ->
        if Signal.Type.is_mem signal then None else Some update)
    in
    let make_read_inputs in_ports =
      List.map in_ports ~f:(fun (name, value_ref) ->
        let fn =
          handle.input_setters
          |> List.find_exn ~f:(fun a -> String.equal (fst a) name)
          |> snd
        in
        fun () -> fn value_ref.contents)
    in
    let clocks =
      let in_ports = String.Map.of_alist_exn in_ports in
      List.filter_map clock_names ~f:(fun name ->
        let%map.Option setter =
          List.find handle.input_setters ~f:(fun (a, _) -> String.equal a name)
        in
        let setter = snd setter in
        let width = Bits.width !(Map.find_exn in_ports name) in
        let gnd = Bits.zero width in
        let vdd = Bits.ones width in
        function
        | `Gnd -> setter gnd
        | `Vdd -> setter vdd)
    in
    let cycle_at_clock_edge () =
      List.iter clocks ~f:(fun f -> f `Gnd);
      handle.eval ();
      List.iter clocks ~f:(fun f -> f `Vdd);
      handle.eval ()
    in
    let cycle_before_clock_edge =
      let set_inputs = make_read_inputs in_ports in
      let read_internals = make_read_internals handle.internal_getters in
      let read_outputs =
        make_read_outputs out_ports_before_clock_edge handle.output_getters
      in
      fun () ->
        List.iter set_inputs ~f:(fun f -> f ());
        handle.eval ();
        List.iter read_outputs ~f:(fun f -> f ());
        List.iter read_internals ~f:(fun f -> f ())
    in
    let cycle_after_clock_edge =
      let read_outputs =
        make_read_outputs out_ports_after_clock_edge handle.output_getters
      in
      fun () -> List.iter read_outputs ~f:(fun f -> f ())
    in
    let complete = handle.complete in
    cycle_before_clock_edge, cycle_at_clock_edge, cycle_after_clock_edge, complete
  in
  let cycle_before_clock_edge, cycle_at_clock_edge, cycle_after_clock_edge, complete =
    make_cycle_functions ()
  in
  let cycle_before_clock_edge = ref cycle_before_clock_edge in
  let cycle_after_clock_edge = ref cycle_after_clock_edge in
  let cycle_at_clock_edge = ref cycle_at_clock_edge in
  let complete = ref complete in
  let reset () =
    !complete ();
    let ( new_cycle_before_clock_edge
        , new_cycle_at_clock_edge
        , new_cycle_after_clock_edge
        , new_complete )
      =
      make_cycle_functions ()
    in
    cycle_after_clock_edge := new_cycle_after_clock_edge;
    cycle_before_clock_edge := new_cycle_before_clock_edge;
    cycle_at_clock_edge := new_cycle_at_clock_edge;
    complete := new_complete
  in
  let cycle_check () = () in
  let internal_memories_table_with_dynamic_lookup =
    Hashtbl.of_alist_exn
      (module Signal.Uid)
      (List.map2_exn read_memories internal_memories ~f:(fun f (_, s, bits) ->
         Signal.uid s, (f, bits)))
  in
  let traced =
    { Cyclesim.Traced.input_ports =
        List.map (Circuit.inputs circuit) ~f:Cyclesim.Traced.to_io_port
    ; output_ports = List.map (Circuit.outputs circuit) ~f:Cyclesim.Traced.to_io_port
    ; internal_signals =
        List.map internal_signals ~f:(fun (signal, names) ->
          { Cyclesim.Traced.signal; mangled_names = names })
    }
  in
  let lookup predicate =
    let map =
      List.fold
        handle.internal_getters
        ~init:(Map.empty (module Signal.Uid))
        ~f:(fun map (_, { signal; bits; update = _; aliases = _ }) ->
          if predicate signal
          then
            Map.add_exn
              map
              ~key:(Signal.uid signal)
              ~data:(Cyclesim.Node.create_from_bits_mutable bits)
          else map)
    in
    fun (traced : Cyclesim.Traced.internal_signal) ->
      Map.find map (Signal.uid traced.signal)
  in
  let lookup_node =
    lookup (fun s -> not (Signal.Type.is_mem s || Signal.Type.is_reg s))
  in
  let lookup_reg traced =
    (* Register in verilator are read-only. *)
    lookup Signal.Type.is_reg traced |> Option.map ~f:Cyclesim.Reg.read_only_of_node
  in
  let lookup_mem (traced : Cyclesim.Traced.internal_signal) =
    Hashtbl.find_and_call
      internal_memories_table_with_dynamic_lookup
      (Signal.uid traced.signal)
      ~if_found:(fun (f, bits_array) ->
        (* Only load the values if needed. *)
        f ();
        Some (Cyclesim.Memory.create_from_read_only_bits_array bits_array))
      ~if_not_found:(Fn.const None)
  in
  Cyclesim.Private.create
    ?circuit:(if config.store_circuit then Some circuit else None)
    ~in_ports
    ~out_ports_before_clock_edge
    ~out_ports_after_clock_edge
    ~reset
    ~cycle_check
    ~cycle_before_clock_edge:(fun () -> !cycle_before_clock_edge ())
    ~cycle_at_clock_edge:(fun () -> !cycle_at_clock_edge ())
    ~cycle_after_clock_edge:(fun () -> !cycle_after_clock_edge ())
    ~traced
    ~lookup_node
    ~lookup_reg
    ~lookup_mem
    ()
;;

module With_interface (I : Hardcaml.Interface.S) (O : Hardcaml.Interface.S) = struct
  module Circuit = Circuit.With_interface (I) (O)

  let create_shared_object
    ?cache:_
    ?build_dir
    ?verilator_config
    ?(config = Cyclesim.Config.default)
    create_fn
    =
    let circuit = Circuit.create_exn ~name:"simulation" create_fn in
    let shared_lib, _verilog_name_by_id =
      compile_circuit_with_cache
        ~cache:No_cache
        ?build_dir
        ?verilator_config
        ~config
        circuit
    in
    shared_lib
  ;;

  let create ?cache ?build_dir ?verilator_config ?config ~clock_names create_fn =
    let circuit = Circuit.create_exn ~name:"simulation" create_fn in
    let ignore_missing_fields t_list ~of_alist list =
      List.map t_list ~f:(fun (n, w) ->
        match List.find list ~f:(fun a -> String.equal n (fst a)) with
        | None -> n, ref (Bits.of_int ~width:w 0)
        | Some x -> x)
      |> of_alist
    in
    Cyclesim.Private.coerce
      (create ?cache ?build_dir ?verilator_config ?config ~clock_names circuit)
      ~to_input:
        (ignore_missing_fields
           (I.to_list I.port_names_and_widths)
           ~of_alist:I.Unsafe_assoc_by_port_name.of_alist)
      ~to_output:
        (ignore_missing_fields
           (O.to_list O.port_names_and_widths)
           ~of_alist:O.Unsafe_assoc_by_port_name.of_alist)
  ;;
end
