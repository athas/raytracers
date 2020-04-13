CC?=gcc
CFLAGS=-O3 -std=c99 -lOpenCL -lm -pthread
FUTHARK_BACKEND?=opencl

all: rgbbox_1000.ppm irreg_1000.ppm

main: main.c ray.h ray.o
	$(CC) -o main $(CFLAGS) main.c ray.o -Wall -Wextra -pedantic

ray.o: ray.h ray.c
	$(CC) -c $(CFLAGS) ray.c

ray.h ray.c: ray.fut
	futhark $(FUTHARK_BACKEND) --library ray.fut

rgbbox_1000.ppm: main
	./main -f $@ -s rgbbox -n 1000 -m 1000

irreg_1000.ppm: main
	./main -f $@ -s irreg -n 1000 -m 1000

clean:
	rm -f ray.h ray.c main
