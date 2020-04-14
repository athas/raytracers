all: rgbbox_1000.ppm irreg_1000.ppm

ray: ray.sml
	polyc ray.sml -o ray

rgbbox_1000.ppm: ray
	./ray -f $@ -s rgbbox -n 1000 -m 1000

irreg_1000.ppm: ray
	./ray -f $@ -s irreg -n 1000 -m 1000

.PHONY: clean

clean:
	rm -f *.ppm
