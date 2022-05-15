package raytracer
import scala.collection.immutable.Nil

final case class AABB(min: Vec3, max: Vec3) {
  inline def surroundingBox(that: AABB): AABB = {
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

  inline def axis(d: Int): Double =
    d % 3 match {
      case 0 => min.x + max.x - min.x
      case 1 => min.y + max.y - min.y
      case 2 => min.z + max.z - min.z
    }
}

sealed abstract class BVH[A] extends Product with Serializable {
  def getAABB: AABB = this match {
    case l: Leaf[A] => l.aabb
    case s: Split[A] => s.aabb
  }
}
final case class Leaf[A](aabb: AABB, a: A) extends BVH[A]
final case class Split[A](aabb: AABB, left: BVH[A], right: BVH[A]) extends BVH[A]

object BVH {
  import java.util.concurrent.{Executors, ForkJoinPool, RecursiveTask}
  import scala.math.Ordering.Double.IeeeOrdering

  private final lazy val pool = Executors.newWorkStealingPool().asInstanceOf[ForkJoinPool]
  sys.addShutdownHook(pool.shutdown())

  def apply[A](f: A => AABB, allObjs: List[A]): BVH[A] = {
    class Go(d: Int, n: Int, objs: List[A]) extends RecursiveTask[BVH[A]] {
      override def compute(): BVH[A] = objs match {
        case Nil => throw new RuntimeException("BVH.apply: empty no nodes")
        case x :: Nil => Leaf(f(x), x)
        case xs =>
          val (xsLeft, xsRight) = xs.sortBy(a => f(a).axis(d)).splitAt(n / 2)
          def doLeft() = new Go(d+1, n/2, xsLeft)
          def doRight() = new Go(d+1, n-n/2, xsRight)
          val l = doLeft().fork()
          val r = doRight().fork()
          val left = l.join()
          val right = r.join()
          val box = left.getAABB.surroundingBox(right.getAABB)
          Split(box, left, right)
      }
    }

    pool.invoke(new Go(0, allObjs.length, allObjs))
  }
}
