# 2-Satisfiability solver

Boolean satisfiability problems are the set of computational problems whose goal is to assign boolean values to a number of variables, in order to satisfy a list of constraints. In this case 2-SAT is the restricted problem where each clause has exactly two variables. 

For example:

<img src="https://render.githubusercontent.com/render/math?math=(x_1 \vee x_2) \wedge (x_3 \vee x_4) \wedge ... \wedge (x_i \vee x_j)">

Despite this restriction 2-SAT allows to solve many different real life problems such as: 

- Conflict-free placement of geometric objects.
- Data clustering.
- Scheduling
- Discrete tomography.
- Inferring business relationships among autonomous subsystems.
- Reconstruction of evolutionary trees.

Many general SAT solvers exist that are not specific to 2-SAT, in many cases implementing Davis–Putnam–Logemann–Loveland (DPLL) algorithm which has <img src="https://render.githubusercontent.com/render/math?math=O(2^N)"> time complexity. However a much faster <img src="https://render.githubusercontent.com/render/math?math=O(N)"> algorithm exists that was designed only for 2-SAT, the Aspvall-Plass-Tarjan (APT) algorithm.

I will try to implement APT in Haskell as a usable package since currently there is no implementation in Haskell, as far as I am aware.

## TODO list

- [x] Algorithm implementation.
- [x] Basic examples.
- [ ] Property testing.
- [ ] Benchmark.
