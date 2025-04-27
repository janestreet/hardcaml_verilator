open Core
open Hardcaml
open Signal
open Utils

let%expect_test "cyclesim with interface" =
  let module I = struct
    type 'a t =
      { clock : 'a
      ; foo : 'a [@bits 16]
      ; bar : 'a [@bits 96]
      }
    [@@deriving hardcaml]
  end
  in
  let module O = struct
    type 'a t =
      { hello : 'a [@bits 16]
      ; world : 'a [@bits 16]
      }
    [@@deriving hardcaml]
  end
  in
  let create (i : _ I.t) =
    { O.hello = reg_fb ~width:16 (Reg_spec.create ~clock:i.clock ()) ~f:(fun x -> x +:. 1)
    ; world = (i.foo +: uresize i.bar ~width:16 +: Signal.(i.bar.:[45, 30]))
    }
  in
  let module Sim = Hardcaml_verilator.With_interface (I) (O) in
  let cache_dir = Filename_unix.temp_dir "" "" in
  let sim =
    Sim.create
      ~cache:(Hashed { cache_dir; raise_if_not_found = false })
      ~verilator_config:Hardcaml_verilator.Config.default
      ~clock_names:[ "clock" ]
      create
  in
  let inputs = Cyclesim.inputs sim in
  let outputs_before = Cyclesim.outputs ~clock_edge:Before sim in
  let outputs_after = Cyclesim.outputs ~clock_edge:After sim in
  inputs.foo := Bits.of_int_trunc ~width:16 1;
  inputs.bar := Bits.of_int_trunc ~width:96 4;
  Cyclesim.cycle sim;
  print_s
    [%message
      ""
        ~hello_before:(Bits.to_int_trunc outputs_before.hello.contents : int)
        ~world_before:(Bits.to_int_trunc outputs_before.world.contents : int)
        ~hello_after:(Bits.to_int_trunc outputs_after.hello.contents : int)
        ~world_after:(Bits.to_int_trunc outputs_after.world.contents : int)];
  [%expect {| ((hello_before 0) (world_before 5) (hello_after 1) (world_after 5)) |}];
  (match Unix.system (sprintf "ls %s" cache_dir) with
   | Ok () -> ()
   | Error e -> raise_s [%message (e : Core_unix.Exit_or_signal.error)]);
  [%expect
    {| c8e5601e4aa8d48e281cbc1dd3e2e441-V5-O3-procs1-threads1-perfile0-perfunc0.so |}]
;;
