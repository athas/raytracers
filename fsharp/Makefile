all: rgbbox_1000.ppm irreg_1000.ppm

ray.exe: ray.fs
	fsharpc ray.fs

rgbbox_1000.ppm: ray.exe
	mono ray.exe -f $@ -s rgbbox -n 1000 -m 1000

irreg_1000.ppm: ray.exe
	mono ray.exe -f $@ -s irreg -n 1000 -m 1000

.PHONY: clean

clean:
	rm -f ray.exe *.ppm
