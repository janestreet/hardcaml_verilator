open Core
open Hardcaml
open Signal

let%expect_test "initial values" =
  let clock = input "clock" 1 in
  let data_width = 8 in
  let address_width = 3 in
  let size = 1 lsl address_width in
  let read_address =
    (* Start a counter at 2 *)
    reg_fb
      (Reg_spec.create ~clock ())
      ~initialize_to:(Bits.of_int_trunc ~width:address_width 2)
      ~width:address_width
      ~f:(fun d -> d +:. 1)
  in
  let ports =
    multiport_memory
      size
      ~write_ports:
        [| { Write_port.write_clock = clock
           ; write_address = input "write_address" address_width
           ; write_data = input "write_data" data_width
           ; write_enable = input "write_enable" 1
           }
        |]
      ~read_addresses:[| read_address |]
      ~initialize_to:(Array.init size ~f:(Bits.of_int_trunc ~width:data_width))
  in
  let circ = Circuit.create_exn ~name:"test" [ output "q" ports.(0) ] in
  let sim = Hardcaml_verilator.create ~clock_names:[ "clock" ] circ in
  let waves, sim = Hardcaml_waveterm.Waveform.create sim in
  for _ = 0 to 1 do
    Cyclesim.cycle sim
  done;
  Cyclesim.in_port sim "write_enable" := Bits.vdd;
  Cyclesim.in_port sim "write_data" := Bits.ones data_width;
  Cyclesim.in_port sim "write_address" := Bits.of_int_trunc ~width:address_width 6;
  Cyclesim.cycle sim;
  Cyclesim.in_port sim "write_enable" := Bits.gnd;
  for _ = 0 to 5 do
    Cyclesim.cycle sim
  done;
  Hardcaml_waveterm.Waveform.print
    ~display_rules:
      [ Hardcaml_waveterm.Display_rule.port_name_matches
          Re.Posix.(compile (re ".*"))
          ~wave_format:(Bit_or Unsigned_int)
          ~alignment:Right
      ]
    waves
    ~wave_width:2;
  [%expect
    {|
    ┌Signals────────┐┌Waves──────────────────────────────────────────────┐
    │clock          ││┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──┐  ┌──│
    │               ││   └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  └──┘  │
    │               ││────────────┬──────────────────────────────────────│
    │write_address  ││ 0          │6                                     │
    │               ││────────────┴──────────────────────────────────────│
    │               ││────────────┬──────────────────────────────────────│
    │write_data     ││ 0          │255                                   │
    │               ││────────────┴──────────────────────────────────────│
    │write_enable   ││            ┌─────┐                                │
    │               ││────────────┘     └────────────────────────────────│
    │               ││──────┬─────┬─────┬─────┬─────┬─────┬─────┬─────┬──│
    │q              ││ 2    │3    │4    │5    │255  │7    │0    │1    │2 │
    │               ││──────┴─────┴─────┴─────┴─────┴─────┴─────┴─────┴──│
    └───────────────┘└───────────────────────────────────────────────────┘
    |}]
;;
