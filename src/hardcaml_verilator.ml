open Core
open Hardcaml
open Ctypes
open Ctypes_foreign_flat
open Foreign
module Unix = Core_unix

module Simulation_backend = struct
  type t =
    | Hardcaml
    | Verilator of { cache_dir : string option }
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
      in
      if use_verilator
      then Verilator { cache_dir = verilator_cache_dir }
      else (
        if Option.is_some verilator_cache_dir
        then
          raise_s
            [%message
              "-verilator-cache-dir can only be specified along with -verilator flag"];
        Hardcaml)]
  ;;
end

let verilator_t : [ `verilator ] structure typ = structure "verilator"

type t =
  { input_setters : (string * (Bits.t -> unit)) list
  ; output_getters : (string * (unit -> Bits.t)) list
  ; eval : unit -> unit
  ; complete : unit -> unit
  }

type 'a with_options =
  ?cache_dir:string
  -> ?build_dir:string
  -> ?verbose:bool
  -> ?optimizations:bool
  -> ?threads:[ `Non_thread_safe | `With_threads of int ]
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

let[@inline always] ceil_div a b = if a % b = 0 then a / b else (a / b) + 1

let run_command_exn ?(verbose = false) command =
  let x = if verbose then command else command ^ " &>/dev/null" in
  match Unix.system x with
  | Ok () -> ()
  | Error e ->
    failwith
      (Sexp.to_string [%message (command : string) (e : Unix.Exit_or_signal.error)])
;;

let generate_cpp_wrapper buffer (circuit : Circuit.t) =
  let circuit_name = Circuit.name circuit in
  let typ = "V" ^ circuit_name in
  bprintf buffer "#include <stdint.h>\n";
  bprintf buffer "#include \"verilated.h\"\n";
  bprintf buffer "#include \"V%s.h\"\n" circuit_name;
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

let create_foreign_bindings ?from (circuit : Circuit.t) =
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
        assert (Bits.width bits = Signal.width input);
        copy_into_verilator (Bits.Expert.unsafe_underlying_repr bits) ))
  in
  let output_getters =
    List.map (Circuit.outputs circuit) ~f:(fun output ->
      let port_name = List.hd_exn (Signal.names output) in
      let size = ceil_div (Signal.width output) 8 in
      let foreign_address =
        foreign
          ?from
          (output_addr_fn_name ~circuit_name ~port_name)
          (ptr verilator_t @-> returning (ptr char))
          verilator_ptr
      in
      let bit_width = Signal.width output in
      let copy_from_verilator =
        copy_to_bytes_from_bigstring
          ~bit_width
          ~src:(Ctypes.bigarray_of_ptr Ctypes.array1 size Bigarray.Char foreign_address)
      in
      ( port_name
      , fun () ->
        let bits = Bits.of_int 0 ~width:bit_width in
        copy_from_verilator (Bits.Expert.unsafe_underlying_repr bits);
        bits ))
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
  { eval; input_setters; output_getters; complete }
;;

let create_foreign_bindings_from_dllib circuit path_to_shared_lib =
  let dllib = Dl.dlopen ~filename:path_to_shared_lib ~flags:[ RTLD_NOW ] in
  create_foreign_bindings ~from:dllib circuit
;;

let compile_circuit
      ?build_dir
      ?(verbose = false)
      ?(optimizations = true)
      ?(threads = `Non_thread_safe)
      ~verilog_contents
      ~circuit
      ()
  =
  let ( ^/ ) = Caml.Filename.concat in
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
    generate_cpp_wrapper buffer circuit;
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
  let relevant_object_files =
    let base = [ "wrapper.o"; "verilated.o" ] in
    match threads with
    | `Non_thread_safe -> base
    | `With_threads _ -> base @ [ "verilated_threads.o" ]
  in
  let optimizations_flag = if optimizations then "-O3" else "-O0" in
  let flags =
    let threads =
      match threads with
      | `Non_thread_safe -> "--no-threads"
      | `With_threads x -> "--threads " ^ Int.to_string x
    in
    let top_module = "--top-module " ^ Circuit.name circuit in
    String.concat ~sep:" " [ optimizations_flag; threads; top_module ]
  in
  run_command_exn
    ~verbose
    (sprintf
       "CXXFLAGS=\"-fPIC\" verilator %s -Wno-COMBDLY -Wno-CMPCONST -Wno-UNSIGNED --cc %s \
        --Mdir %s %s"
       flags
       path_to_verilog
       obj_dir
       path_to_cpp_wrapper);
  run_command_exn
    ~verbose
    (sprintf
       "CXXFLAGS=\"-fPIC -g %s\" make -C %s -f V%s.mk V%s__ALL.a %s"
       optimizations_flag
       obj_dir
       (Circuit.name circuit)
       (Circuit.name circuit)
       (String.concat ~sep:" " relevant_object_files));
  run_command_exn
    ~verbose
    (sprintf
       "g++ -lpthread -fPIC %s -g -shared -o %s %s %s"
       optimizations_flag
       path_to_shared_lib
       (String.concat
          ~sep:" "
          (List.map ~f:(fun a -> obj_dir ^/ a) relevant_object_files))
       path_to_static_lib);
  path_to_shared_lib
;;

let compile_circuit_with_cache
      ?cache_dir
      ?build_dir
      ?verbose
      ?optimizations
      ?threads
      circuit
  =
  let shared_lib =
    let verilog_contents =
      let buffer = Buffer.create 32 in
      Rtl.output ~output_mode:(Rtl.Output_mode.To_buffer buffer) Verilog circuit;
      buffer
    in
    let compile_circuit () =
      compile_circuit
        ?build_dir
        ?verbose
        ?optimizations
        ?threads
        ~circuit
        ~verilog_contents
        ()
    in
    match cache_dir with
    | None -> compile_circuit ()
    | Some cache_dir ->
      let md5 = Md5.digest_bytes (Buffer.contents_bytes verilog_contents) in
      let option_to_string f a =
        match a with
        | None -> "none"
        | Some a -> "some-" ^ f a
      in
      let threads_to_string t =
        match t with
        | `Non_thread_safe -> "non-thread-safe"
        | `With_threads x -> sprintf "with-threads-%d" x
      in
      let fname =
        let suffix =
          [ "optimizations"
          ; option_to_string Bool.to_string optimizations
          ; "threads"
          ; option_to_string threads_to_string threads
          ]
          |> String.concat ~sep:"-"
        in
        cache_dir ^/ Md5_lib.to_hex md5 ^ "-" ^ suffix ^ ".so"
      in
      run_command_exn ?verbose (sprintf "mkdir -p %s" cache_dir);
      if Sys_unix.file_exists_exn fname
      then fname
      else (
        let compiled = compile_circuit () in
        run_command_exn ?verbose (sprintf "cp %s %s" compiled fname);
        fname)
  in
  shared_lib
;;

let compile_circuit_and_load_shared_object
      ?cache_dir
      ?build_dir
      ?verbose
      ?optimizations
      ?threads
      circuit
  =
  let shared_lib =
    compile_circuit_with_cache
      ?cache_dir
      ?build_dir
      ?verbose
      ?optimizations
      ?threads
      circuit
  in
  create_foreign_bindings_from_dllib circuit shared_lib
;;

let create ?cache_dir ?build_dir ?verbose ?optimizations ?threads ~clock_names circuit =
  let shared_object =
    compile_circuit_with_cache
      ?cache_dir
      ?build_dir
      ?verbose
      ?optimizations
      ?threads
      circuit
  in
  let make_port_list signals =
    List.map signals ~f:(fun s ->
      List.hd_exn (Signal.names s), ref (Bits.of_int ~width:(Signal.width s) 0))
  in
  let in_ports = make_port_list (Circuit.inputs circuit) in
  let out_ports_before_clock_edge = make_port_list (Circuit.outputs circuit) in
  let out_ports_after_clock_edge = make_port_list (Circuit.outputs circuit) in
  let make_cycle_functions () =
    let handle = create_foreign_bindings_from_dllib circuit shared_object in
    let make_read_outputs out_ports =
      List.map out_ports ~f:(fun (name, value_ref) ->
        let fn =
          handle.output_getters
          |> List.find_exn ~f:(fun a -> String.equal (fst a) name)
          |> snd
        in
        fun () -> value_ref := fn ())
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
      let read_outputs = make_read_outputs out_ports_before_clock_edge in
      fun () ->
        List.iter set_inputs ~f:(fun f -> f ());
        handle.eval ();
        List.iter read_outputs ~f:(fun f -> f ())
    in
    let cycle_after_clock_edge =
      let read_outputs = make_read_outputs out_ports_after_clock_edge in
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
  Cyclesim.Private.create
    ~in_ports
    ~out_ports_before_clock_edge
    ~out_ports_after_clock_edge
    ~internal_ports:[]
    ~reset
    ~cycle_check
    ~cycle_before_clock_edge:(fun () -> !cycle_before_clock_edge ())
    ~cycle_at_clock_edge:(fun () -> !cycle_at_clock_edge ())
    ~cycle_after_clock_edge:(fun () -> !cycle_after_clock_edge ())
    ~lookup_signal:(fun _ -> failwith "lookup_reg unsupported in verilator simulator yet")
    ~lookup_reg:(fun _ -> failwith "lookup_reg unsupported in verilator simulator yet")
    ~assertions:(Map.empty (module String))
;;

module With_interface (I : Hardcaml.Interface.S) (O : Hardcaml.Interface.S) = struct
  module Circuit = Circuit.With_interface (I) (O)

  let create ?cache_dir ?build_dir ?verbose ?optimizations ?threads ~clock_names create_fn
    =
    let circuit = Circuit.create_exn ~name:"simulation" create_fn in
    let ignore_missing_fields t_list ~of_alist list =
      List.map t_list ~f:(fun (n, w) ->
        match List.find list ~f:(fun a -> String.equal n (fst a)) with
        | None -> n, ref (Bits.of_int ~width:w 0)
        | Some x -> x)
      |> of_alist
    in
    Cyclesim.Private.coerce
      (create ?cache_dir ?build_dir ?verbose ?optimizations ?threads ~clock_names circuit)
      ~to_input:(ignore_missing_fields (I.to_list I.t) ~of_alist:I.of_alist)
      ~to_output:(ignore_missing_fields (O.to_list O.t) ~of_alist:O.of_alist)
  ;;
end
