# Memoized SPL Analyses

This is a branch on a forked version of the source code repo by Ramy Shahin
(https://github.com/ramyshahin/ProductLineAnalysis)

This branch supports memoization to store results from previous analyses.

## Building and running

Built with `stack-2.9.3` and `ghc-8.6.5`.

* You need to install the `libz3-dev` and `cudd-3.0.0` library (https://github.com/ivmai/cudd)

Running the analyses:

* For `Return`, `ReturnAvg`, `DanglingSwitch` and `CaseTermination`:

```sh
# Replace 'RETURN' by 'RETURN_AVG', 'DANGLING_SWITCH` or `CALL_DENSITY` for other analyses (needs stack clean first):
stack build --ghc-options -DRETURN
stack run
```

* For `CaseTermination` and `GotosDensity`:
```sh
# Gotos
stack runghc benchmarks/ControlFlow/app/MainGotos.hs
# CaseTermination
stack runghc benchmarks/ControlFlow/app/MainCaseTermination.hs
```