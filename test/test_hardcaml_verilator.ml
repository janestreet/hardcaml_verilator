open Core
open Hardcaml
open Signal
module Unix = Core_unix

let set_input (handle : Hardcaml_verilator.t) name =
  snd (List.find_exn ~f:(fun (a, _) -> String.equal name a) handle.input_setters)
;;

let get_output (handle : Hardcaml_verilator.t) name =
  snd (List.find_exn ~f:(fun (a, _) -> String.equal name a) handle.output_getters)
;;

let%expect_test "combinatorial and threaded" =
  let circuit =
    let adder_output =
      let a = input "a" 16 in
      let b = input "b" 39 in
      output "c" (a +: uresize b 16)
    in
    Circuit.create_exn ~name:"adder" [ adder_output ]
  in
  let handle = Hardcaml_verilator.compile_circuit_and_load_shared_object circuit in
  let set_input = set_input handle in
  let get_output = get_output handle in
  set_input "a" (Bits.of_int ~width:16 1);
  set_input "b" (Bits.of_int ~width:39 9);
  handle.eval ();
  printf "Output = %d\n" (Bits.to_int (get_output "c" ()));
  [%expect {| Output = 10 |}]
;;

let%expect_test "sequential" =
  let circuit =
    let counter_output =
      let clock = input "clock" 1 in
      let clear = input "clear" 1 in
      let reg_spec = Reg_spec.create ~clock ~clear () in
      reg_fb reg_spec ~w:8 ~enable:vdd (fun a -> a +:. 1) |> output "value"
    in
    Circuit.create_exn ~name:"counter" [ counter_output ]
  in
  let handle = Hardcaml_verilator.compile_circuit_and_load_shared_object circuit in
  let set_input = set_input handle in
  let get_output = get_output handle in
  let set_clock = set_input "clock" in
  let set_clear = set_input "clear" in
  let get_value = get_output "value" in
  let eval = handle.eval in
  set_clear Bits.gnd;
  set_clock Bits.vdd;
  for i = 1 to 6 do
    eval ();
    set_clock Bits.gnd;
    eval ();
    printf "[%d (before)] Value = %d\n" i (Bits.to_int (get_value ()));
    set_clock Bits.vdd;
    eval ();
    printf "[%d (after)] Value = %d\n" i (Bits.to_int (get_value ()))
  done;
  [%expect
    {|
    [1 (before)] Value = 0
    [1 (after)] Value = 1
    [2 (before)] Value = 1
    [2 (after)] Value = 2
    [3 (before)] Value = 2
    [3 (after)] Value = 3
    [4 (before)] Value = 3
    [4 (after)] Value = 4
    [5 (before)] Value = 4
    [5 (after)] Value = 5
    [6 (before)] Value = 5
    [6 (after)] Value = 6 |}]
;;

let%expect_test "cyclesim" =
  let clock = input "clk" 1 in
  let a = input "a" 16 in
  let b = input "b" 16 in
  let foo = output "foo" (a +: b) in
  let bar = output "bar" (reg (Reg_spec.create ~clock ()) ~enable:vdd (a +: b)) in
  let circuit = Circuit.create_exn ~name:"adder" [ foo; bar ] in
  let sim = Hardcaml_verilator.create ~clock_names:[ "clk" ] circuit in
  let () =
    let inputs = Cyclesim.inputs sim in
    let find name = List.find_exn inputs ~f:(fun a -> String.equal name (fst a)) |> snd in
    find "a" := Bits.of_int ~width:16 10;
    find "b" := Bits.of_int ~width:16 20
  in
  Cyclesim.cycle sim;
  let () =
    let find clock_edge name =
      let outputs = Cyclesim.outputs ~clock_edge sim in
      (List.find_exn outputs ~f:(fun a -> String.equal name (fst a)) |> snd).contents
      |> Bits.to_int
    in
    let foo_before = find Before "foo" in
    let bar_before = find Before "bar" in
    let foo_after = find After "foo" in
    let bar_after = find After "bar" in
    print_s
      [%message (foo_before : int) (bar_before : int) (foo_after : int) (bar_after : int)]
  in
  ();
  [%expect {| ((foo_before 30) (bar_before 0) (foo_after 30) (bar_after 30)) |}]
;;

let%expect_test "cyclesim with interface" =
  let module I = struct
    type 'a t =
      { clock : 'a
      ; foo : 'a [@bits 16]
      ; bar : 'a [@bits 96]
      }
    [@@deriving sexp_of, hardcaml]
  end
  in
  let module O = struct
    type 'a t =
      { hello : 'a [@bits 16]
      ; world : 'a [@bits 16]
      }
    [@@deriving sexp_of, hardcaml]
  end
  in
  let create (i : _ I.t) =
    { O.hello =
        reg_fb ~enable:vdd ~w:16 (Reg_spec.create ~clock:i.clock ()) (fun x -> x +:. 1)
    ; world = (i.foo +: uresize i.bar 16 +: Signal.(i.bar.:[45, 30]))
    }
  in
  let module Sim = Hardcaml_verilator.With_interface (I) (O) in
  let cache_dir = Filename_unix.temp_dir "" "" in
  let sim =
    Sim.create ~cache_dir ~threads:`Non_thread_safe ~clock_names:[ "clock" ] create
  in
  let inputs = Cyclesim.inputs sim in
  let outputs_before = Cyclesim.outputs ~clock_edge:Before sim in
  let outputs_after = Cyclesim.outputs ~clock_edge:After sim in
  inputs.foo := Bits.of_int ~width:16 1;
  inputs.bar := Bits.of_int ~width:96 4;
  Cyclesim.cycle sim;
  print_s
    [%message
      ""
        ~hello_before:(Bits.to_int outputs_before.hello.contents : int)
        ~world_before:(Bits.to_int outputs_before.world.contents : int)
        ~hello_after:(Bits.to_int outputs_after.hello.contents : int)
        ~world_after:(Bits.to_int outputs_after.world.contents : int)];
  [%expect {| ((hello_before 0) (world_before 5) (hello_after 1) (world_after 5)) |}];
  (match Unix.system (sprintf "ls %s" cache_dir) with
   | Ok () -> ()
   | Error e -> raise_s [%message (e : Core_unix.Exit_or_signal.error)]);
  [%expect
    {| cf6c2ce492346d42c55a7fa7fcf78dd2-optimizations-none-threads-some-non-thread-safe.so |}]
;;

let%expect_test "test all port sizes" =
  let inputs =
    List.init 128 ~f:(fun i -> input ("input" ^ Int.to_string (i + 1)) (i + 1))
  in
  let outputs =
    List.mapi inputs ~f:(fun i j -> output ("output" ^ Int.to_string (i + 1)) ~:j)
  in
  let circuit = Circuit.create_exn ~name:"port_sizes" outputs in
  let sim = Hardcaml_verilator.create ~clock_names:[] circuit in
  let inputs =
    List.init 128 ~f:(fun i -> Cyclesim.in_port sim ("input" ^ Int.to_string (i + 1)))
  in
  let outputs =
    List.init 128 ~f:(fun i -> Cyclesim.out_port sim ("output" ^ Int.to_string (i + 1)))
  in
  let random_inputs () = List.init 128 ~f:(fun i -> Bits.random ~width:(i + 1)) in
  for _ = 0 to 9 do
    let inputs' = random_inputs () in
    List.iter2_exn inputs inputs' ~f:( := );
    Cyclesim.cycle sim;
    let all_outputs = List.map outputs ~f:(fun o -> !o) |> Bits.concat_msb in
    if not (Bits.equal all_outputs (Bits.( ~: ) (Bits.concat_msb inputs')))
    then print_s [%message "mismatch"]
  done
;;
