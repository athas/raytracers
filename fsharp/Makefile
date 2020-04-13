all: rgbbox_1000.ppm irreg_1000.ppm

rgbbox_1000.ppm:
	dotnet run -c release -- -f $@ -s rgbbox -n 1000 -m 1000

irreg_1000.ppm:
	dotnet run -c release -- -f $@ -s irreg -n 1000 -m 1000

.PHONY: clean

clean:
	dotnet clean
	rm -f *.ppm
