all: rgbbox_1000.ppm irreg_1000.ppm

rgbbox_1000.ppm:
	cabal run ray -- rgbbox 1000 1000
	mv rgbbox.png $@

irreg_1000.ppm:
	cabal run ray -- irreg 1000 1000
	mv irreg.png $@

bench:
	cabal bench

.PHONY: clean

clean:
	cabal clean
	rm -f *.ppm
