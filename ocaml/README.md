# Multicore OCaml implementation 

Multicore OCaml is still in development while this code is being written, so bad performance should 
be taken lightly.

## Requisites

Install Opam and make the development Multicore OCaml `switch`:

* Opam is the package manager of OCaml: https://opam.ocaml.org/doc/Install.html
  * Multicore OCaml can be gotten in its development state using Opam: https://github.com/ocaml-multicore/multicore-opam

Also:
```opam install ocamlbuild domainslib```

## Running

Just `make ray.native`. The generated `ray.native` takes the following options,
all of which are optional:

* `-m height`
* `-n width`
* `-f file.ppm`
* `-s <rgbbox|irreg>`
* `--cores number-of-cores`
* `--chunk-size-render chunk-size-of-pixels`
