.PHONY: bench clean

bench:
	mix deps.get
	MIX_ENV=prod mix run bench.exs

raytracer:
	mix deps.get
	MIX_ENV=prod mix escript.build

rgbbox.ppm: raytracer
	./raytracer --width 200 --height 200 --file-out $@ rgbbox

irreg.ppm: raytracer
	./raytracer --width 200 --height 200 --file-out $@ irreg

clean:
	rm -rf raytracer deps _build rgbbox.ppm irreg.ppm
