open Core
open Hardcaml
open Signal

let test ~which_simulator ~extended_ports =
  let ex n = n ^ "%" ^ n in
  let p n = if extended_ports then ex n else n in
  let i = input (p "i") 1 in
  let x = ~:i -- ex "x" in
  let o = output (p "o") x in
  let circuit = Circuit.create_exn ~name:"foo" [ o ] in
  let sim =
    match which_simulator with
    | `Cyclesim -> Cyclesim.create ~config:Cyclesim.Config.trace_all circuit
    | `Verilator ->
      Hardcaml_verilator.create ~clock_names:[] ~config:Cyclesim.Config.trace_all circuit
  in
  let waves, sim = Hardcaml_waveterm.Waveform.create sim in
  let testbench () =
    let i = Cyclesim.in_port sim (p "i") in
    let o = Cyclesim.out_port sim (p "o") in
    let x = Cyclesim.lookup_node_or_reg_by_name sim (ex "x") in
    i := Bits.vdd;
    Cyclesim.cycle sim;
    print_s [%message (o : Bits.t ref) (x : Cyclesim.Node.t option)];
    i := Bits.gnd;
    Cyclesim.cycle sim;
    print_s [%message (o : Bits.t ref) (x : Cyclesim.Node.t option)];
    Hardcaml_waveterm.Waveform.print waves
  in
  testbench ()
;;

let%expect_test "cyclesim with extended names for reference" =
  test ~which_simulator:`Cyclesim ~extended_ports:false;
  [%expect
    {|
    ((o 0) (x (0)))
    ((o 1) (x (1)))
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │i              ││────────┐                                          │
    │               ││        └───────                                   │
    │o              ││        ┌───────                                   │
    │               ││────────┘                                          │
    │x%x            ││        ┌───────                                   │
    │               ││────────┘                                          │
    └───────────────┘└───────────────────────────────────────────────────┘
    |}];
  test ~which_simulator:`Cyclesim ~extended_ports:true;
  [%expect
    {|
    ((o 0) (x (0)))
    ((o 1) (x (1)))
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │i%i            ││────────┐                                          │
    │               ││        └───────                                   │
    │o%o            ││        ┌───────                                   │
    │               ││────────┘                                          │
    │x%x            ││        ┌───────                                   │
    │               ││────────┘                                          │
    └───────────────┘└───────────────────────────────────────────────────┘
    |}]
;;

let%expect_test "verilator with extended names" =
  test ~which_simulator:`Verilator ~extended_ports:false;
  [%expect
    {|
    ((o 0) (x ()))
    ((o 1) (x ()))
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │i              ││────────┐                                          │
    │               ││        └───────                                   │
    │o              ││        ┌───────                                   │
    │               ││────────┘                                          │
    │\x%x           ││        ┌───────                                   │
    │               ││────────┘                                          │
    └───────────────┘└───────────────────────────────────────────────────┘
    |}];
  if false then test ~which_simulator:`Verilator ~extended_ports:true
;;
