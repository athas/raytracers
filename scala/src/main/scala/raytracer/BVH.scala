package raytracer
import scala.collection.immutable.Nil

final case class AABB(min: Vec3, max: Vec3) {
  def surroundingBox(that: AABB): AABB = {
    val small =
      Vec3( math.min(this.min.x, that.min.x)
          , math.min(this.min.y, that.min.y)
          , math.min(this.min.z, that.min.z)
          )
    val big =
      Vec3( math.max(this.max.x, that.max.x)
          , math.max(this.max.y, that.max.y)
          , math.max(this.max.z, that.max.z)
          )
    AABB(small, big)
  }

  def axis(d: Int): Float =
    d % 3 match {
      case 0 => centre.x
      case 1 => centre.y
      case 2 => centre.z
    }

  def centre: Vec3 =
    Vec3( min.x + max.x - min.x
        , min.y + max.y - min.y
        , min.z + max.z - min.z
        )
}

sealed trait BVH[A] {
  def getAABB: AABB = this match {
    case Leaf(aabb, _) => aabb
    case Split(aabb, _, _) => aabb
  }
}
final case class Leaf[A](aabb: AABB, a: A) extends BVH[A]
final case class Split[A](aabb: AABB, left: BVH[A], right: BVH[A]) extends BVH[A]

object BVH {
  import scala.math.Ordering.Float.IeeeOrdering
  def apply[A](f: A => AABB, allObjs: List[A]): BVH[A] = {
    def go(d: Int, n: Int, objs: List[A]): BVH[A] = objs match {
      case Nil => throw new RuntimeException("BVH.apply: empty no nodes")
      case x :: Nil => Leaf(f(x), x)
      case xs => {
        val (xsLeft, xsRight) =
          xs.sortBy(a => f(a).axis(d))
            .splitAt(n / 2)
          val left = go(d+1, n / 2, xsLeft)
          val right = go(d+1, n - n / 2, xsRight)
          val box = left.getAABB.surroundingBox(right.getAABB)
          Split(box, left, right)
      }
    }
    go(0, allObjs.length, allObjs)
  }
}
