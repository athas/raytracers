all: rgbbox_1000.ppm irreg_1000.ppm

rgbbox_1000.ppm: src/main/scala/raytracer/*
	sbt "run rgbbox 1000 1000"
	mv out.ppm $@

irreg_1000.ppm: src/main/scala/raytracer/*
	sbt "run irreg 1000 1000"
	mv out.ppm $@

.PHONY: clean

clean:
	sbt clean
	rm -f *.ppm

bench:
	sbt clean bench
