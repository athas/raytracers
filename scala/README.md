- Make sure you have sbt installed
- run `sbt`
- then run `run <scene> <width> <height>`
- the output file will be written to `out.ppm`

## Benchmark custom JVM version

Use [Jabba](https://github.com/shyiko/jabba) to install a custom JVM version.
For example, to run the benchmarks with GraalVM.

```sh
curl -sL https://github.com/shyiko/jabba/raw/master/install.sh | bash && . ~/.jabba/jabba.sh
jabba install graalvm@20.0.0
jabba use graalvm@20.0.0
make bench
```
