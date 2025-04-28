open Core
open Hardcaml
open Signal

let%expect_test "cyclesim" =
  let clock = input "clk" 1 in
  let a = input "a" 16 in
  let b = input "b" 16 in
  let foo = output "foo" (a +: b) in
  let bar = output "bar" (reg (Reg_spec.create ~clock ()) ~enable:vdd (a +: b)) in
  let circuit = Circuit.create_exn ~name:"adder" [ foo; bar ] in
  let sim = Hardcaml_verilator.create ~clock_names:[ "clk" ] circuit in
  let test () =
    let inputs = Cyclesim.inputs sim in
    let find name = List.find_exn inputs ~f:(fun a -> String.equal name (fst a)) |> snd in
    find "a" := Bits.of_int_trunc ~width:16 10;
    find "b" := Bits.of_int_trunc ~width:16 20;
    Cyclesim.cycle sim;
    let find clock_edge name =
      let outputs = Cyclesim.outputs ~clock_edge sim in
      (List.find_exn outputs ~f:(fun a -> String.equal name (fst a)) |> snd).contents
      |> Bits.to_int_trunc
    in
    let foo_before = find Before "foo" in
    let bar_before = find Before "bar" in
    let foo_after = find After "foo" in
    let bar_after = find After "bar" in
    print_s
      [%message (foo_before : int) (bar_before : int) (foo_after : int) (bar_after : int)]
  in
  test ();
  [%expect {| ((foo_before 30) (bar_before 0) (foo_after 30) (bar_after 30)) |}];
  test ();
  [%expect {| ((foo_before 30) (bar_before 30) (foo_after 30) (bar_after 30)) |}];
  (* Reset brings us back to the initial state. *)
  Cyclesim.reset sim;
  test ();
  [%expect {| ((foo_before 30) (bar_before 0) (foo_after 30) (bar_after 30)) |}]
;;
