CC?=clang
FUTHARK_BACKEND?=multicore

ifeq ($(FUTHARK_BACKEND),opencl)
CFLAGS=-O3 -std=c99
LDFLAGS=-lm -lOpenCL
else ifeq ($(FUTHARK_BACKEND),multicore)
CFLAGS=-O3 -std=c99
LDFLAGS=-lm -lpthread
else ifeq ($(FUTHARK_BACKEND),cuda)
CFLAGS=-O3 -std=c99 -lm
LDFLAGS=-lcuda -lnvrtc
else
CFLAGS=-O3 -std=c99 -lm
LDFLAGS=-lm
endif

all: rgbbox_1000.ppm irreg_1000.ppm

main: main.c ray.o
	$(CC) -o main $(CFLAGS) $(LDFLAGS) $^ -Wall -Wextra -pedantic

ray.o: ray.c
	$(CC) -c $(CFLAGS) $^

ray.c: *.fut
	futhark $(FUTHARK_BACKEND) --library ray.fut

rgbbox_1000.ppm: main
	./main -f $@ -s rgbbox -n 1000 -m 1000

irreg_1000.ppm: main
	./main -f $@ -s irreg -n 1000 -m 1000

clean:
	rm -f ray.h ray.c main
