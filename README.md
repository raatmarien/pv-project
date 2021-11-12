# bounded-verification

This is the wlp-based bounded symbolic verification tool by Mark van
de Meer, Samuel Klumpers, Philipp Zander and Marien Raat. 

## Tests

Run the tests with

```shell
stack test
```

## Verifying

You can see all the options with:

```shell
stack run -- --help
```

For example to verify memberOf.gcl you can run:

```shell
stack run -- --program test/examples/benchmark/memberOf.gcl --n 10 --k 30
```

This will print `Nothing` because there is no counter example

To check the mutations, simply add `--mutate`:

```shell
stack run -- --program test/examples/benchmark/memberOf.gcl --n 10 --k 30 --mutate
```

You can disable the pruning with `--disableOptimizations`:

```shell
stack run -- --program test/examples/benchmark/memberOf.gcl --n 10 --k 30 --disableOptimizations
```

You can run the benchmarks with the `--benchmark` option, this generates a csv
file with the values, which you can pretify using the included pythons script.
 
```shell
stack run -- --program test/examples/benchmark/memberOf.gcl --n 10 --k 30 --benchmark
```

## Work done

Samuel: Implement z3 connection, implement wlp, implement pointers

Philipp: Create datastructures, implement pruning, implement pointers

Mark: Arrays verification, related works in paper, testing

Marien: Path generation, wlp to z3, arrays verification, benchmarking
implementation, implementation in paper, performance section in paper, related
works in paper, unit tests
