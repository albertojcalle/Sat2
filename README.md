# 2sat

Boolean satisfiability problems are the set of computational problems which goal is to assign boolean values to a number of variables, in order to satisfy a list of constraints. In this case 2-satisfiability or 2-SAT is the restricted problem where each constraint has exactly two variables. 

Despite this restriction 2-sat allows to solve many different real life problems such as: conflict-free placement of geometric objects, data clustering, scheduling, discrete tomography, inferring business relationships among autonomous subsystems of the internet, and reconstruction of evolutionary trees.

Many general solvers exist that are not specific to 2-sat, in many cases implementing DPLL algorithm. However a much faster algorithm exists that was designed only for 2-SAT, the Aspvall-Plass-Tarjan algorithm.

We will try to implement APT in Haskell, not only as an exercise, but as a usable library since currently there is no implementation in haskell.

## Linux installation

### Fedora

We need to install some aditional packages:

sudo dnf install ghc-gtk-devel
