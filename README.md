# CS675: Hello SMT

This repository contains example code to introduce the use of the Z3 SMT Solver API.

## Building and Running

1. First you will need to install Z3 on your computer. The [Z3 website](https://github.com/Z3Prover/z3). You can download a pre-packaged release from the releases tab. Make sure you have z3 or z3.exe (if you are using Windows) in your PATH and (if you are using Linux) libz3.so and libz3java.so in your LD\_LIBRARY\_PATH.


2. First run SBT.

    $ sbt


3. Then run the command set fork := true within SBT. This is required to prevent running into certain weird bugs in involving SBT and its use of DLLs.

    sbt:cs675-hello-smt> set fork := true

4. Now you can compile.

    sbt:cs675-hello-smt> compile

5. And then run your code!

    sbt:cs675-hello-smt> run


