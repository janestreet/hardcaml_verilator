open Core
open Hardcaml
open Signal

module Alternating = struct
  module I = struct
    type 'a t =
      { clock : 'a
      ; clear : 'a
      }
    [@@deriving hardcaml]
  end

  module O = struct
    type 'a t =
      { value : 'a [@bits 1]
      ; counter1 : 'a [@bits 3]
      ; counter2 : 'a [@bits 3]
      }
    [@@deriving hardcaml]
  end

  module State = struct
    type t =
      | First
      | Second
    [@@deriving compare, enumerate, sexp_of]
  end

  let create (i : Signal.t I.t) =
    let spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
    let sm = Always.State_machine.create (module State) spec ~enable:vdd in
    let c1 = Always.Variable.reg spec ~enable:vdd ~width:3 in
    let c1_next = c1.value +:. 1 in
    let c2 = Always.Variable.reg spec ~enable:vdd ~width:3 in
    let c2_next = c2.value +:. 1 in
    let w1 = Always.Variable.wire ~default:gnd in
    ignore (sm.current -- "STATE" : Signal.t);
    ignore (w1.value -- "W1" : Signal.t);
    ignore (c1.value -- "C1" : Signal.t);
    ignore (c2.value -- "C2" : Signal.t);
    ignore (c1_next -- "C1_NEXT" : Signal.t);
    ignore (c2_next -- "C2_NEXT" : Signal.t);
    Always.(
      compile
        [ sm.switch
            [ ( First
              , [ c1 <-- c1_next
                ; when_ (c1.value ==:. 1) [ c1 <--. 0; sm.set_next Second ]
                ] )
            ; ( Second
              , [ c2 <-- c2_next
                ; w1 <-- vdd
                ; when_ (c2.value ==:. 2) [ c2 <--. 0; sm.set_next First ]
                ] )
            ]
        ]);
    { O.value = w1.value; counter1 = c1.value; counter2 = c2.value }
  ;;
end

let%expect_test "waveform with internal signals (state machine)" =
  let open Hardcaml_waveterm in
  let module Sim = Hardcaml_verilator.With_interface (Alternating.I) (Alternating.O) in
  let sim =
    Sim.create
      ~clock_names:[ "clock" ]
      ~config:Cyclesim.Config.trace_all
      Alternating.create
  in
  let i = Cyclesim.inputs sim in
  let outputs_before = Cyclesim.outputs ~clock_edge:Before sim in
  let outputs_after = Cyclesim.outputs ~clock_edge:After sim in
  let waveform, sim = Waveform.create sim in
  i.clear := Bits.gnd;
  for _ = 1 to 12 do
    Cyclesim.cycle sim;
    print_s
      [%message
        ""
          ~value_before:(Bits.to_int_trunc outputs_before.value.contents : int)
          ~counter1_before:(Bits.to_int_trunc outputs_before.counter1.contents : int)
          ~counter2_before:(Bits.to_int_trunc outputs_before.counter2.contents : int)
          ~value_after:(Bits.to_int_trunc outputs_after.value.contents : int)
          ~counter1_after:(Bits.to_int_trunc outputs_after.counter1.contents : int)
          ~counter2_after:(Bits.to_int_trunc outputs_after.counter2.contents : int)]
  done;
  [%expect
    {|
    ((value_before 0) (counter1_before 0) (counter2_before 0) (value_after 0)
     (counter1_after 1) (counter2_after 0))
    ((value_before 0) (counter1_before 1) (counter2_before 0) (value_after 1)
     (counter1_after 0) (counter2_after 0))
    ((value_before 1) (counter1_before 0) (counter2_before 0) (value_after 1)
     (counter1_after 0) (counter2_after 1))
    ((value_before 1) (counter1_before 0) (counter2_before 1) (value_after 1)
     (counter1_after 0) (counter2_after 2))
    ((value_before 1) (counter1_before 0) (counter2_before 2) (value_after 0)
     (counter1_after 0) (counter2_after 0))
    ((value_before 0) (counter1_before 0) (counter2_before 0) (value_after 0)
     (counter1_after 1) (counter2_after 0))
    ((value_before 0) (counter1_before 1) (counter2_before 0) (value_after 1)
     (counter1_after 0) (counter2_after 0))
    ((value_before 1) (counter1_before 0) (counter2_before 0) (value_after 1)
     (counter1_after 0) (counter2_after 1))
    ((value_before 1) (counter1_before 0) (counter2_before 1) (value_after 1)
     (counter1_after 0) (counter2_after 2))
    ((value_before 1) (counter1_before 0) (counter2_before 2) (value_after 0)
     (counter1_after 0) (counter2_after 0))
    ((value_before 0) (counter1_before 0) (counter2_before 0) (value_after 0)
     (counter1_after 1) (counter2_after 0))
    ((value_before 0) (counter1_before 1) (counter2_before 0) (value_after 1)
     (counter1_after 0) (counter2_after 0))
    |}];
  Waveform.expect ~display_width:80 ~wave_width:1 waveform;
  [%expect
    {|
    ┌Signals───────────┐┌Waves─────────────────────────────────────────────────────┐
    │clock             ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─│
    │                  ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ │
    │clear             ││                                                          │
    │                  ││────────────────────────────────────────────────          │
    │                  ││────┬───┬───────────────┬───┬───────────────┬───          │
    │counter1          ││ 0  │1  │0              │1  │0              │1            │
    │                  ││────┴───┴───────────────┴───┴───────────────┴───          │
    │                  ││────────────┬───┬───┬───────────┬───┬───┬───────          │
    │counter2          ││ 0          │1  │2  │0          │1  │2  │0                │
    │                  ││────────────┴───┴───┴───────────┴───┴───┴───────          │
    │value             ││        ┌───────────┐       ┌───────────┐                 │
    │                  ││────────┘           └───────┘           └───────          │
    │                  ││────┬───┬───────────────┬───┬───────────────┬───          │
    │C1                ││ 0  │1  │0              │1  │0              │1            │
    │                  ││────┴───┴───────────────┴───┴───────────────┴───          │
    │                  ││────┬───┬───────────────┬───┬───────────────┬───          │
    │C1_NEXT           ││ 1  │2  │1              │2  │1              │2            │
    │                  ││────┴───┴───────────────┴───┴───────────────┴───          │
    │                  ││────────────┬───┬───┬───────────┬───┬───┬───────          │
    │C2                ││ 0          │1  │2  │0          │1  │2  │0                │
    │                  ││────────────┴───┴───┴───────────┴───┴───┴───────          │
    │                  ││────────────┬───┬───┬───────────┬───┬───┬───────          │
    │C2_NEXT           ││ 1          │2  │3  │1          │2  │3  │1                │
    │                  ││────────────┴───┴───┴───────────┴───┴───┴───────          │
    │                  ││────────┬───────────┬───────┬───────────┬───────          │
    │STATE             ││ First  │Second     │First  │Second     │First            │
    │                  ││────────┴───────────┴───────┴───────────┴───────          │
    │W1                ││        ┌───────────┐       ┌───────────┐                 │
    │                  ││────────┘           └───────┘           └───────          │
    │gnd               ││                                                          │
    │                  ││────────────────────────────────────────────────          │
    │vdd               ││────────────────────────────────────────────────          │
    │                  ││                                                          │
    └──────────────────┘└──────────────────────────────────────────────────────────┘
    d45932c606eff424a4e4c1f15e414220
    |}]
;;
