CC?=gcc
FUTHARK_BACKEND?=opencl

ifeq ($(FUTHARK_BACKEND),opencl)
CFLAGS=-O3 -std=c99 -lm -lOpenCL
else ifeq ($(FUTHARK_BACKEND),multicore)
CFLAGS=-O3 -std=c99 -lm -pthread
else ifeq ($(FUTHARK_BACKEND),cuda)
CFLAGS=-O3 -std=c99 -lm -lcuda -lnvrtc
else
CFLAGS=-O3 -std=c99 -lm
endif

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
