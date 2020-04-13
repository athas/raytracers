package raytracer

import scala.collection.parallel.immutable._

final case class Image(width: Int, height: Int, pixels: Image.PixelData) {
  def toPPM: String = {
    def pixel2ppm(p: Image.Pixel): String =
      s"${p._1 & 0xFF} ${p._2 & 0xFF} ${p._3 & 0xFF}"

    (Seq("P3", s"$width $height", "255") ++
      pixels.flatten.map(pixel2ppm))
        .mkString("\n")
  }
}

object Image {
  type Pixel = (Byte, Byte, Byte)
  type PixelData = ParSeq[ParSeq[Pixel]]

  def apply(width: Int, height: Int, pixel: (Int, Int) => Pixel): Image =
    Image(width, height, ParSeq.tabulate(height, width)(pixel))

}

