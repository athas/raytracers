# Futhark implementation

Because we want to benchmark the rendering time independently of the
BVH construction and scene creation, we don't use the normal `futhark
bench` tool.  Instead we have a C program, `main.c` that calls the
Futhark program as a library.  Run `make` to build and then benchmark
with e.g.

```
$ ./main -n 1000 -m 1000 -s rgbbox -f rgbbox.ppm
```

This will also create an output image.
