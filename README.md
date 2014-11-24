Requirements
------------

- [Racket](http://download.racket-lang.org)
- [Rosette](http://github.com/emina/rosette)


Getting Started
---------------

You can run Racket programs using command line (i.e. `racket your_program.rkt`) or via DrRacket.

`test-simulator.rkt` is an example program that interprets an ARM assembly program on a particular input program state and a symbolic program state.

`test-solver.rkt` is an example program that check the equivalence of two ARM assembly programs.

Note that we currently support subset of ARM instructions (e.g. most of 32-bit instructions).
