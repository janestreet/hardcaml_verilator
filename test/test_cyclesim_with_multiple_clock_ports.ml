open Core
open Hardcaml
open Signal

module I = struct
  type 'a t =
    { clock_foo : 'a
    ; clock_bar : 'a
    ; a : 'a [@bits 16]
    ; b : 'a [@bits 16]
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { foo : 'a [@bits 16]
    ; bar : 'a [@bits 16]
    }
  [@@deriving hardcaml]
end

module V = Hardcaml_verilator.With_interface (I) (O)

let%expect_test "Cyclesim with two clock port" =
  List.iter Bool.all ~f:(fun specify_clock_names ->
    let create_fn { I.clock_foo; clock_bar; a; b } =
      let foo = reg (Reg_spec.create ~clock:clock_foo ()) (a -: b) in
      let bar = reg (Reg_spec.create ~clock:clock_bar ()) (a +: b) in
      { O.foo; bar }
    in
    let sim =
      V.create
        ?clock_names:
          (if specify_clock_names
           then Some [ I.port_names.clock_foo; I.port_names.clock_bar ]
           else None)
        create_fn
    in
    let test () =
      let inputs = Cyclesim.inputs sim in
      let outputs_before = Cyclesim.outputs ~clock_edge:Before sim in
      let outputs_after = Cyclesim.outputs ~clock_edge:After sim in
      inputs.a := Bits.of_unsigned_int ~width:16 20;
      inputs.b := Bits.of_unsigned_int ~width:16 10;
      Cyclesim.cycle sim;
      let foo_before = Bits.to_unsigned_int !(outputs_before.foo) in
      let bar_before = Bits.to_unsigned_int !(outputs_before.bar) in
      let foo_after = Bits.to_unsigned_int !(outputs_after.foo) in
      let bar_after = Bits.to_unsigned_int !(outputs_after.bar) in
      print_s
        [%message
          (foo_before : int) (bar_before : int) (foo_after : int) (bar_after : int)]
    in
    test ();
    [%expect {| ((foo_before 0) (bar_before 0) (foo_after 10) (bar_after 30)) |}];
    test ();
    [%expect {| ((foo_before 10) (bar_before 30) (foo_after 10) (bar_after 30)) |}];
    (* Reset brings us back to the initial state. *)
    Cyclesim.reset sim;
    test ();
    [%expect {| ((foo_before 0) (bar_before 0) (foo_after 10) (bar_after 30)) |}])
;;
