package raytracer

import org.openjdk.jmh.annotations._

import java.util.concurrent.TimeUnit

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
class Bench {

  val width = 1000
  val height = 1000
  @Param(Array("rgbbox", "irreg"))
  var scene: String = _

  var objscam: (Raytracer.Objs, Raytracer.Camera) = _
  var s: Scene = _

  @Setup
  def setup(): Unit = {
    s = Scene.fromString(scene).get
    objscam = s.toObjsCam(width, height)
  }

  @Benchmark def construct(): Unit = s.toObjsCam(width, height)
  @Benchmark def render(): Unit = Raytracer.render(objscam._1, width, height, objscam._2)
}
