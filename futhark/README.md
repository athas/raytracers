# Futhark implementation

Benchmark with

```
$ futhark bench --backend=opencl .
```

Due to Futhark's limitations and sharp edges, it's a bit more tricky
to get an output image out of it.  You'll need
[data2png.py](https://github.com/diku-dk/futhark/blob/master/tools/data2png.py)
and then something like:

```
echo 500 500 | ./ray -e render_rgbbox -b | rocm ~/repos/futhark/tools/data2png.py /dev/stdin rgbbox.png
```
