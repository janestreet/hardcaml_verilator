(* Test looking up different node types within the simuator.

   Verilator has some restrictions over the hardcaml simulation which are demonstrated
   here.
*)

open Core
open Hardcaml
open Signal

let test width =
  let module I = struct
    type 'a t =
      { clock : 'a
      ; we : 'a
      ; d : 'a [@bits width]
      ; ra : 'a [@bits 2]
      }
    [@@deriving hardcaml]
  end
  in
  let module O = struct
    type 'a t = { q : 'a [@bits width] } [@@deriving hardcaml]
  end
  in
  let create { I.clock; we; d; ra } =
    let spec = Reg_spec.create ~clock () in
    let wa =
      let w = wire 2 in
      let r = reg spec ~enable:we w in
      w <== r +:. 1;
      r
    in
    let q =
      (multiport_memory
         ~name:"q_mem"
         4
         ~write_ports:
           [| { write_clock = clock
              ; write_data = d
              ; write_enable = we
              ; write_address = wa
              }
           |]
         ~read_addresses:[| ra |]).(0)
    in
    let q = q +:. 10 -- "q_comb" in
    let q = reg spec q -- "q_reg" in
    { O.q }
  in
  let module Vsim = Hardcaml_verilator.With_interface (I) (O) in
  let open Expect_test_helpers_core in
  let sim =
    Vsim.create ~clock_names:[ "clock" ] ~config:Cyclesim.Config.trace_all create
  in
  (* read internal nodes *)
  require_does_not_raise [%here] (fun () ->
    let q_comb = Cyclesim.lookup_node_by_name sim "q_comb" |> Option.value_exn in
    assert (Bits.width (Cyclesim.Node.to_bits q_comb) = width));
  (* read internal regs *)
  require_does_not_raise [%here] (fun () ->
    let q_reg = Cyclesim.lookup_reg_by_name sim "q_reg" |> Option.value_exn in
    assert (Bits.width (Cyclesim.Reg.to_bits q_reg) = width));
  (* cannot write internal regs *)
  require_does_raise [%here] (fun () ->
    let q_reg = Cyclesim.lookup_reg_by_name sim "q_reg" |> Option.value_exn in
    Cyclesim.Reg.of_bits q_reg (Bits.of_int ~width 10));
  (* read internal memories *)
  require_does_not_raise [%here] (fun () ->
    let q_mem = Cyclesim.lookup_mem_by_name sim "q_mem" |> Option.value_exn in
    assert (Bits.width (Cyclesim.Memory.to_bits ~address:1 q_mem) = width));
  (* cannot write internal memories *)
  require_does_raise [%here] (fun () ->
    let q_mem = Cyclesim.lookup_mem_by_name sim "q_mem" |> Option.value_exn in
    Cyclesim.Memory.of_bits q_mem ~address:1 (Bits.of_int ~width 10))
;;

let%expect_test "lookup simulator - 1 bit" =
  test 1;
  [%expect
    {|
    "Cannot set simulation node - read-only"
    "[Cyclesim_lookup.Memory.unsafe_set64] memory node is read-only"
    |}]
;;

let%expect_test "lookup simulator - 7 bit" =
  test 7;
  [%expect
    {|
    "Cannot set simulation node - read-only"
    "[Cyclesim_lookup.Memory.unsafe_set64] memory node is read-only"
    |}]
;;

let%expect_test "lookup simulator - 8 bit" =
  test 8;
  [%expect
    {|
    "Cannot set simulation node - read-only"
    "[Cyclesim_lookup.Memory.unsafe_set64] memory node is read-only"
    |}]
;;

let%expect_test "lookup simulator - 16 bit" =
  test 16;
  [%expect
    {|
    "Cannot set simulation node - read-only"
    "[Cyclesim_lookup.Memory.unsafe_set64] memory node is read-only"
    |}]
;;

let%expect_test "lookup simulator - 17 bit" =
  test 17;
  [%expect
    {|
    "Cannot set simulation node - read-only"
    "[Cyclesim_lookup.Memory.unsafe_set64] memory node is read-only"
    |}]
;;

let%expect_test "lookup simulator - 31 bit" =
  test 31;
  [%expect
    {|
    "Cannot set simulation node - read-only"
    "[Cyclesim_lookup.Memory.unsafe_set64] memory node is read-only"
    |}]
;;

let%expect_test "lookup simulator - 32 bit" =
  test 32;
  [%expect
    {|
    "Cannot set simulation node - read-only"
    "[Cyclesim_lookup.Memory.unsafe_set64] memory node is read-only"
    |}]
;;
