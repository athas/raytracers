package raytracer

final case class Vec3(x: Float, y: Float, z: Float) {

  def +(that: Vec3) = Vec3(x + that.x, y + that.y, z + that.z)
  def -(that: Vec3) = Vec3(x - that.x, y - that.y, z - that.z)
  def *(that: Vec3) = Vec3(x * that.x, y * that.y, z * that.z)

  def scale(a: Float): Vec3 = Vec3(a * x, a * y, a * z)
  def dot(that: Vec3): Float =
    x * that.x + y * that.y + z * that.z
  def norm: Float = math.sqrt(this dot this).toFloat
  def normalise: Vec3 = scale (1.0f / norm)
  def cross(that: Vec3): Vec3 =
    Vec3(y * that.z - z * that.y, z * that.x - x * that.z, x * that.y - y * that.x)

}

object Vec3 {
  val one = Vec3(1, 1, 1)
}
