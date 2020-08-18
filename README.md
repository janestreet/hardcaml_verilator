"Hardcaml Verilator"
====================

Hardcaml_verilator converts Hardcaml designs to verilog and compiles
them with [verilator](https://www.veripool.org/wiki/verilator). This
produces a very high performance, cycle accurate, simulation model of
the design.

The library transparently compiles the verilator simulation model to a
shared library and loads it into the running program. It exposes a
simulation API compatible with [Hardcaml.Cyclesim].

Compiling the verilator simulation model can take significant time -
therefore a simple caching scheme is implemented so that the shared
library can be reused on the second and subsequent runs if the design
does not change.
