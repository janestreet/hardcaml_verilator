module Unix = Core_unix

val set_input : Hardcaml_verilator.t -> string -> Hardcaml_verilator.input_port
val get_output : Hardcaml_verilator.t -> string -> Hardcaml_verilator.output_port
