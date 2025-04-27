open Core
open Hardcaml
open Signal
open Utils

let%expect_test "sequential" =
  let circuit =
    let counter_output =
      let clock = input "clock" 1 in
      let clear = input "clear" 1 in
      let reg_spec = Reg_spec.create ~clock ~clear () in
      reg_fb reg_spec ~width:8 ~f:(fun a -> a +:. 1) |> output "value"
    in
    Circuit.create_exn ~name:"counter" [ counter_output ]
  in
  let handle =
    let show_command_output = false in
    Hardcaml_verilator.compile_circuit_and_load_shared_object
      ~verilator_config:
        { Hardcaml_verilator.Config.default with verbose = show_command_output }
      circuit
  in
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
    printf "[%d (before)] Value = %d\n" i (Bits.to_int_trunc (get_value ()));
    set_clock Bits.vdd;
    eval ();
    printf "[%d (after)] Value = %d\n" i (Bits.to_int_trunc (get_value ()))
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
    [6 (after)] Value = 6
    |}]
;;
