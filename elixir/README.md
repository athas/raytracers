# Raytracer

An elixir implementation of a Raytracer.

## Prerequisites

You will need elixir and erlang. Using Nix, the following should be enough:

```
nix-shell -p erlang elixir
```

## Usage

### Running benchmarks

```
make bench
```

### Generating the output images

```
make rgbbox.ppm
make irreg.ppm
```
