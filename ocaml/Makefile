all: rgbbox_1000.ppm irreg_1000.ppm

ray.native: ray.ml
	ocamlbuild ray.native

rgbbox_1000.ppm: ray.native
	./ray.native -f $@ -s rgbbox -n 1000 -m 1000

irreg_1000.ppm: ray.native
	./ray.native -f $@ -s irreg -n 1000 -m 1000

.PHONY: clean

clean:
	rm -rf ray.native *.ppm _build
