package raytracer

import scala.collection.parallel.immutable._

final case class Image(width: Int, height: Int, pixels: Image.PixelData) {
  def toPPM: String = {
    def pixel2ppm(p: Image.Pixel): String =
      s"${p._1 & 0xFF} ${p._2 & 0xFF} ${p._3 & 0xFF}"

    (Seq("P3", s"$width $height", "255") ++
      pixels.map(pixel2ppm))
        .mkString("\n") + "\n"
  }
}

object Image {
  type Pixel = (Byte, Byte, Byte)
  type PixelData = ParSeq[Pixel]

  def apply(width: Int, height: Int, pixel: (Int, Int) => Pixel): Image = {
    val arr = ParSeq.tabulate(width * height)(n =>
        (height - n / height, n % width))
    val pixelData = arr.map(pixel.tupled)
    Image(width, height, pixelData)
  }
}

