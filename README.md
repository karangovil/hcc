# HCC - a Haskell C compiler

An incremental (growing slowly!) compiler for C written in Haskell

## Requirements
* Install Haskell via stack

## Usage
* Build hcc using stack
```
stack install
````
Note that stack install will put the executable on the path whereas stack build will only generate the executable in the `.local\bin` directory. the executable `hcc` expects the stack exectuable (`hcc-exe`) to be on the path.

* To run it, invoke the `hcc` executable in the project root
```
./hcc /path/to/source.c
```
The compiled executable will be in the same directory as the source file, and have the same name (e.g. `source` in the example above).

## Tests
* Unit tests  are written using [HSpec](https://hspec.github.io/) and can be run using stack
```
stack test
```

* Sample programs are included in `examples` and can be run using `test-examples.sh` script
```
./test-examples.sh
```
These tests compare the executables against `gcc`.