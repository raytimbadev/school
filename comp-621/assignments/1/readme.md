N-body Simulation
=================

To run the simulation, run `matlab` in the `src` directory, and invoke the
`manyruns` function with the number of runs to perform. This will run the
simulation with the number of bodies to simulate set to `15` and the amount of
time to simulate set to `2` seconds. (That's the number of *simulated* seconds,
not the number of wall-clock seconds!) Those are the values used to generate
the data given in the report. They can be adjusted in the file `manyruns.m`.

The simulations are deterministic, due to the use of the same seed for the
random number generator. The initial conditions of the simulation are chosen
randomly, in particular:

  * the initial positions of the bodies
  * the initial momenta of the bodies
  * the masses of the bodies

The seed can be adjusted in the file `kernel.m` (or `kernel_opt.m` for the
optimized code).
