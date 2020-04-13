CC?=gcc
CFLAGS=-O3 -std=c99 -lOpenCL -lm -pthread
FUTHARK_BACKEND?=opencl

all: main

main: main.c ray.h ray.o
	$(CC) -o main $(CFLAGS) main.c ray.o -Wall -Wextra -pedantic

ray.o: ray.h ray.c
	$(CC) -c $(CFLAGS) ray.c

ray.h ray.c: ray.fut
	futhark $(FUTHARK_BACKEND) --library ray.fut

clean:
	rm -f ray.h ray.c main
