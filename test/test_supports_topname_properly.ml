open Core
open Hardcaml
open Signal

(* When internal signals are enabled we look up some internal data structures of the
   simulation object. They are named according to the [circuit_name]. Make sure that
   it still works. *)
let%expect_test ("Supports topname properly" [@tags "disabled"]) =
  let module Sim = Hardcaml_verilator in
  let i = input "i" 1 in
  let n = ~:i -- "foo" in
  let o = output "o" n in
  let circ = Circuit.create_exn ~name:"blah" [ o ] in
  ignore
    (Sim.create ~config:Cyclesim.Config.trace_all ~clock_names:[] circ
     : Cyclesim.t_port_list)
;;
