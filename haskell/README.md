# Haskell implementation

## TL;DR

```
$ cabal bench --benchmark-options="+RTS -N8"
```

Replace `8` with how many physical threads you want to use.

## Details

This program makes use of the cabal-install build tool (the command
line program is just called `cabal`).  You will need at least `cabal`
3.0.0.0.

There are two programs here: `ray`, which is used for generating
images, and `bench-ray`, which is a benchmarking harness that makes
use of the excellent `criterion` library.  First you must run the
following command:

```
cabal update
```

Then you can build both by running

```
cabal build --enable-benchmarks
```

For running the programs, use `cabal run`.  For example, to generate
a 400x400 image of the "rgbbox" scene:

```
cabal run -v0 ray -- rgbbox 400 400 > out.ppm
```

For running the benchmarks:

```
cabal run -v0 bench-ray --
```

You can also specify only a subset of benchmarks to run; for example
to run only the BVH calculation benchmarks:

```
cabal run -v0 bench-ray bvh --
```

## Running with multiple threads

To run with multiple threads, place `+RTS -Nx` at the end of the
command line, where *x* is the number of threads to use, e.g:

```
cabal run -v0 bench-ray bvh -- +RTS -N4
```

It is **crucial** that the `RTS` options go after `--`, as otherwise
they will be consumed by the `cabal` executable itself.
