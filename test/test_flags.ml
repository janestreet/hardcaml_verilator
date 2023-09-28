open Core
open Hardcaml_verilator

let obj_dir = "obj_dir"
let circuit_name = "circuit"
let path_to_verilog = "verilog"
let path_to_cpp_wrapper = "wrapper"
let path_to_shared_lib = "shared"
let path_to_static_lib = "static"

let verilator_compilation_command (config : Config.t) =
  let executable =
    match config.verilator_version with
    | V4 -> "verilator4"
    | V5 -> "verilator5"
  in
  Config.verilator_compilation_command
    config
    ~executable
    ~circuit_name
    ~path_to_verilog
    ~obj_dir
    ~path_to_cpp_wrapper
;;

let make_compilation_command config =
  Config.make_compilation_command config ~circuit_name ~obj_dir
;;

let final_link_command config =
  Config.final_link_command config ~obj_dir ~path_to_shared_lib ~path_to_static_lib
;;

let print config =
  Stdio.print_endline (verilator_compilation_command config);
  Stdio.print_endline (make_compilation_command config);
  Stdio.print_endline (final_link_command config)
;;

let%expect_test "labels" =
  Stdio.print_string Config.(label default);
  [%expect {| V5-O3-procs1-threads1-perfile0-perfunc0 |}];
  Stdio.print_string Config.(label small_cfiles);
  [%expect {| V5-O3-procs1-threads1-perfile5000-perfunc500 |}]
;;

let%expect_test "default - v5" =
  print Config.default;
  [%expect
    {|
    CXXFLAGS="-fPIC" verilator5 -O3 --threads 1 --top-module circuit  -Wno-COMBDLY -Wno-CMPCONST -Wno-UNSIGNED --cc verilog  --Mdir obj_dir wrapper
    CXXFLAGS="-fPIC -g -O3" OBJCACHE=""  make  -C obj_dir -f Vcircuit.mk Vcircuit__ALL.a wrapper.o verilated.o verilated_threads.o
    g++ -lpthread -fPIC -O3 -g -shared -o shared obj_dir/wrapper.o obj_dir/verilated.o obj_dir/verilated_threads.o static |}]
;;

let%expect_test "small_cfiles - v5" =
  print Config.small_cfiles;
  [%expect
    {|
    CXXFLAGS="-fPIC" verilator5 -O3 --threads 1 --top-module circuit --output-split  5000 --output-split-cfuncs  500 --output-split-ctrace 500 -Wno-COMBDLY -Wno-CMPCONST -Wno-UNSIGNED --cc verilog  --Mdir obj_dir wrapper
    CXXFLAGS="-fPIC -g -O3" OBJCACHE=""  make  -C obj_dir -f Vcircuit.mk Vcircuit__ALL.a wrapper.o verilated.o verilated_threads.o
    g++ -lpthread -fPIC -O3 -g -shared -o shared obj_dir/wrapper.o obj_dir/verilated.o obj_dir/verilated_threads.o static |}]
;;

let%expect_test "small_cfiles parallel - v5" =
  print { Config.small_cfiles with compilation_processes = Threads.create 27 };
  [%expect
    {|
    CXXFLAGS="-fPIC" verilator5 -O3 --threads 1 --top-module circuit --output-split  5000 --output-split-cfuncs  500 --output-split-ctrace 500 -Wno-COMBDLY -Wno-CMPCONST -Wno-UNSIGNED --cc verilog  --Mdir obj_dir wrapper
    CXXFLAGS="-fPIC -g -O3" OBJCACHE="" VM_PARALLEL_BUILDS=1 make -j27 -C obj_dir -f Vcircuit.mk Vcircuit__ALL.a wrapper.o verilated.o verilated_threads.o
    g++ -lpthread -fPIC -O3 -g -shared -o shared obj_dir/wrapper.o obj_dir/verilated.o obj_dir/verilated_threads.o static |}]
;;

let print (config : Config.t) = print { config with verilator_version = V4 }

let%expect_test "default - v4" =
  print Config.default;
  [%expect
    {|
    CXXFLAGS="-fPIC" verilator4 -O3 --no-threads --top-module circuit  -Wno-COMBDLY -Wno-CMPCONST -Wno-UNSIGNED --cc verilog  --Mdir obj_dir wrapper
    CXXFLAGS="-fPIC -g -O3" OBJCACHE=""  make  -C obj_dir -f Vcircuit.mk Vcircuit__ALL.a wrapper.o verilated.o
    g++ -lpthread -fPIC -O3 -g -shared -o shared obj_dir/wrapper.o obj_dir/verilated.o static |}]
;;

let%expect_test "small_cfiles - v4" =
  print Config.small_cfiles;
  [%expect
    {|
    CXXFLAGS="-fPIC" verilator4 -O3 --no-threads --top-module circuit --output-split  5000 -Wno-COMBDLY -Wno-CMPCONST -Wno-UNSIGNED --cc verilog  --Mdir obj_dir wrapper
    CXXFLAGS="-fPIC -g -O3" OBJCACHE=""  make  -C obj_dir -f Vcircuit.mk Vcircuit__ALL.a wrapper.o verilated.o
    g++ -lpthread -fPIC -O3 -g -shared -o shared obj_dir/wrapper.o obj_dir/verilated.o static |}]
;;

let%expect_test "parallel - v4" =
  print { Config.default with compilation_processes = Threads.create 33 };
  [%expect
    {|
    CXXFLAGS="-fPIC" verilator4 -O3 --no-threads --top-module circuit  -Wno-COMBDLY -Wno-CMPCONST -Wno-UNSIGNED --cc verilog  --Mdir obj_dir wrapper
    CXXFLAGS="-fPIC -g -O3" OBJCACHE="" VM_PARALLEL_BUILDS=1 make -j33 -C obj_dir -f Vcircuit.mk Vcircuit__ALL.a wrapper.o verilated.o
    g++ -lpthread -fPIC -O3 -g -shared -o shared obj_dir/wrapper.o obj_dir/verilated.o static |}]
;;
