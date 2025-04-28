open Core
module Unix = Core_unix

let set_input (handle : Hardcaml_verilator.t) name =
  snd (List.find_exn ~f:(fun (a, _) -> String.equal name a) handle.input_setters)
;;

let get_output (handle : Hardcaml_verilator.t) name =
  snd (List.find_exn ~f:(fun (a, _) -> String.equal name a) handle.output_getters)
;;
