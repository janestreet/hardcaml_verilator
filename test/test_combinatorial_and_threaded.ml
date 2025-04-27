open Core
open Hardcaml
open Signal
open Utils

let%expect_test "combinatorial and threaded" =
  let circuit =
    let adder_output =
      let a = input "a" 16 in
      let b = input "b" 39 in
      output "c" (a +: uresize b ~width:16)
    in
    Circuit.create_exn ~name:"adder" [ adder_output ]
  in
  let handle = Hardcaml_verilator.compile_circuit_and_load_shared_object circuit in
  let set_input = set_input handle in
  let get_output = get_output handle in
  set_input "a" (Bits.of_int_trunc ~width:16 1);
  set_input "b" (Bits.of_int_trunc ~width:39 9);
  handle.eval ();
  printf "Output = %d\n" (Bits.to_int_trunc (get_output "c" ()));
  [%expect {| Output = 10 |}]
;;
