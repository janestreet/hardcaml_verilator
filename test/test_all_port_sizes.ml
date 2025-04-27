open Core
open Hardcaml
open Signal

let%expect_test "test all port sizes" =
  let inputs =
    List.init 128 ~f:(fun i -> input ("input" ^ Int.to_string (i + 1)) (i + 1))
  in
  let outputs =
    List.mapi inputs ~f:(fun i j -> output ("output" ^ Int.to_string (i + 1)) ~:j)
  in
  let circuit = Circuit.create_exn ~name:"port_sizes" outputs in
  let sim = Hardcaml_verilator.create ~clock_names:[] circuit in
  let inputs =
    List.init 128 ~f:(fun i -> Cyclesim.in_port sim ("input" ^ Int.to_string (i + 1)))
  in
  let outputs =
    List.init 128 ~f:(fun i -> Cyclesim.out_port sim ("output" ^ Int.to_string (i + 1)))
  in
  let random_inputs () = List.init 128 ~f:(fun i -> Bits.random ~width:(i + 1)) in
  for _ = 0 to 9 do
    let inputs' = random_inputs () in
    List.iter2_exn inputs inputs' ~f:( := );
    Cyclesim.cycle sim;
    let all_outputs = List.map outputs ~f:(fun o -> !o) |> Bits.concat_msb in
    if not (Bits.equal all_outputs (Bits.( ~: ) (Bits.concat_msb inputs')))
    then print_s [%message "mismatch"]
  done
;;
