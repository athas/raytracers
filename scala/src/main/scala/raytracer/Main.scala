package raytracer

import scala.util.Try
import raytracer.Raytracer.render

object Main {
  def main(args: Array[String]): Unit = {
    val (scene, width, height) = (args match {
      case Array(s, w, h, _*) => for {
        ss <- Scene.fromString(s)
        ww <- Try(w.toInt).toOption
        hh <- Try(h.toInt).toOption
      } yield (ss, ww, hh)
      case _ => None
    }).getOrElse({
      println("Error parsing command line arguments, running rgbbox 1000x1000 px")
      (Scene.rgbbox, 1000, 1000)
    })

    val (objs, cam) = scene.toObjsCam(width, height)
    val image = render(objs, width, height, cam)
    image.write()
  }
}
