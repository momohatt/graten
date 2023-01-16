# graten: Gradual Tensor Shape Checking

[![test](https://github.com/momohatt/graten/actions/workflows/test.yml/badge.svg)](https://github.com/momohatt/graten/actions/workflows/test.yml)

## How to run

### Prerequisites

System requirements:

* z3 >= 4.8.3 (make sure to have it in your PATH)
* stack

### How to build (with stack)

```
$ stack init  # first time only
$ stack build
$ stack exec -- graten --help       # show options
$ stack exec -- graten filename.ml
```

### How to run tests

You can run all tests with the following command.
```
$ stack test
```

To run one test, execute the following command.
```
$ stack test --ta=--select-tests=test/fixture/tensor.ml
```
