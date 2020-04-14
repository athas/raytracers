package raytracer

final case class Vec3(x: Float, y: Float, z: Float) {

  @inline def +(that: Vec3) = Vec3(x + that.x, y + that.y, z + that.z)
  @inline def -(that: Vec3) = Vec3(x - that.x, y - that.y, z - that.z)
  @inline def *(that: Vec3) = Vec3(x * that.x, y * that.y, z * that.z)

  @inline def scale(a: Float): Vec3 = Vec3(a * x, a * y, a * z)
  @inline def dot(that: Vec3): Float =
    x * that.x + y * that.y + z * that.z
  @inline def norm: Float = math.sqrt(this dot this).toFloat
  @inline def normalise: Vec3 = scale (1.0f / norm)
  @inline def cross(that: Vec3): Vec3 =
    Vec3(y * that.z - z * that.y, z * that.x - x * that.z, x * that.y - y * that.x)

}

object Vec3 {
  val one = Vec3(1, 1, 1)
}
