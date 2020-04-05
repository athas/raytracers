CC?=gcc
CFLAGS=-O -std=c99 -lOpenCL -lm

main: main.c ray.h ray.o
	$(CC) -o main $(CFLAGS) main.c ray.o -Wall -Wextra -pedantic

ray.o: ray.h ray.c
	$(CC) -c $(CFLAGS) ray.c

ray.h ray.c: ray.fut
	futhark opencl --library ray.fut

clean:
	rm -f ray.h ray.c main
