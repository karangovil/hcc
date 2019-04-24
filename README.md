# HCC - a Haskell C compiler

An incremental (growing slowly!) compiler for C written in Haskell

## Requirements
* Install Haskell via stack

## Usage
* Build it
```
stack build
````

* To run it, invoke the `hcc` executable via `stack exec` in the project root
```
stack exec hcc-exe /path/to/source.c
```
The compiled executable will be in the same directory as the source file, and have the same name (e.g. `source` in the example above).

## Tests
[Add tests]
