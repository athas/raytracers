package raytracer

import scala.collection.parallel.immutable.ParSeq

final case class Image(width: Int, height: Int, pixels: Image.PixelData) {
  import java.io.PrintWriter
  import scala.util.Using

  def write(): Unit = Using.resource(new PrintWriter("out.ppm")) { pw =>
    val writePPM = (p: Image.Pixel) => pw.write(s"${p.x & 0xFF} ${p.y & 0xFF} ${p.z & 0xFF}\n")
    pw.write(s"P3\n$width $height\n255\n")
    pixels.seq.foreach(writePPM)
  }
}

object Image {
  type PixelData = ParSeq[Pixel]

  def apply(width: Int, height: Int, pixel: (Int, Int) => Pixel): Image = {
    val arr = ParSeq.tabulate(width * height)(n => (height - n / height, n % width))
    val pixelData = arr.map(pixel.tupled)
    Image(width, height, pixelData)
  }

  class Pixel(val x: Byte, val y: Byte, val z: Byte)
}