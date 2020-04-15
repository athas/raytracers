package raytracer

object Raytracer {

  type Pos = Vec3
  type Dir = Vec3
  val pi = math.Pi.toDouble

  type Color = Vec3
  val black: Color = Vec3(0, 0, 0)
  val white: Color = Vec3(1, 1, 1)

  final case class Ray(origin: Pos, dir: Dir) {

    @inline def pointAtParam(t: Double): Pos = origin + dir.scale(t)

    def aabbHit(aabb: AABB, tMin0: Double, tMax0: Double): Boolean = {
      def go(min_ : Double, max_ : Double, origin_ : Double, dir_ : Double, tMin_ : Double, tMax_ : Double): (Double, Double) = {
        val invD = 1f / dir_
        val t0 = (min_ - origin_) * invD
        val t1 = (max_ - origin_) * invD
        val (t0_, t1_) = if (invD < 0) (t1, t0) else (t0, t1)
        val tMin__ = t0_ max tMin_
        val tMax__ = t1_ min tMax_
        (tMin__, tMax__)
      }
      val (tMin1, tMax1) = go(aabb.min.x, aabb.max.x, origin.x, dir.x, tMin0, tMax0)
      if(tMax1 <= tMin1) false
      else {
        val (tMin2, tMax2) = go(aabb.min.y, aabb.max.y, origin.y, dir.y, tMin1, tMax1)
        if(tMax2 <= tMin2) false
        else {
          val (tMin3, tMax3) = go(aabb.min.z, aabb.max.z, origin.z, dir.z, tMin2, tMax2)
          !(tMax3 <= tMin3)
        }
      }
    }

  }
  final case class Hit(t: Double, p: Pos, normal: Dir, color: Color)
  final case class Sphere(pos: Pos, color: Color, radius: Double) {
    def aabb: AABB = AABB(
      pos - Vec3(radius, radius, radius),
      pos + Vec3(radius, radius, radius))

    def hit(ray: Ray, tMin: Double, tMax: Double): Option[Hit] = {
      val oc = ray.origin - pos
      val a = ray.dir dot ray.dir
      val b = oc dot ray.dir
      val c = (oc dot oc) - radius * radius
      val discriminant = b*b - a*c
      def tryHit(temp: Double) =
        if(temp < tMax && temp > tMin)
          Some(
            Hit( temp
               , ray.pointAtParam(temp)
               , (ray.pointAtParam(temp) - pos).scale(1f / radius)
               , color
               ))
        else None

      if(discriminant <= 0) None
      else tryHit((-b - math.sqrt(b*b - a*c).toDouble) / a) match {
        case s: Some[Hit] => s
        case None => tryHit((-b + math.sqrt(b*b - a*c).toDouble)/a)
      }
    }

  }

    

  type Objs = BVH[Sphere]

  def objsHit(objs: Objs, ray: Ray, tMin: Double, tMax: Double): Option[Hit] = objs match {
    case Leaf(_, sphere) =>
      sphere.hit(ray, tMin, tMax)
    case Split(box, left, right) =>
      if(!ray.aabbHit(box, tMin, tMax)) None
      else objsHit(left, ray, tMin, tMax) match {
        case Some(hit1) => Some(objsHit(right, ray, tMin, hit1.t).getOrElse(hit1))
        case None => objsHit(right, ray, tMin, tMax)
      }
  }

  final case class Camera(origin: Pos, llc: Pos, horizontal: Dir, vertical: Dir)

  object Camera {
    def apply(lookFrom: Pos, lookAt: Pos, vUp: Dir, vFov: Double, aspect: Double): Camera = {
      val theta = vFov * pi / 180f
      val halfHeight = math.tan(theta / 2f).toDouble
      val halfWidth = aspect * halfHeight
      val origin = lookFrom
      val w = (lookFrom - lookAt).normalise
      val u = (vUp cross w).normalise
      val v = w cross u
      Camera( lookFrom
            , origin - u.scale(halfWidth) - v.scale(halfHeight) - w
            , u.scale(2*halfWidth)
            , v.scale(2*halfHeight)
            )
    }
  }

  def getRay(cam: Camera, s: Double, t: Double): Ray =
    Ray( cam.origin
       , cam.llc + cam.horizontal.scale(s) + cam.vertical.scale(t) - cam.origin
       )

  @inline def reflect(v: Vec3, n: Vec3): Vec3 =
    v - n.scale(2 * (v dot n))

  @inline def scatter(ray: Ray, hit: Hit): Option[(Ray, Vec3)] = {
    val reflected = reflect(ray.dir.normalise, hit.normal)
    val scattered = Ray(hit.p, reflected)
    if((reflected dot hit.normal) > 0)
      Some((scattered, hit.color)) else None
  }

  def rayColor(objs: Objs, ray: Ray, depth: Int): Color = objsHit(objs, ray, 0.001f, (1.0f / 0.0f)) match {
    case Some(hit) => scatter(ray, hit) match {
      case Some((scattered, attenuation)) if depth < 50 =>
        attenuation * rayColor(objs, scattered, depth+1)
      case _ => black
    }
    case None =>
      val unitDir = ray.dir.normalise
      val t = 0.5f * (unitDir + Vec3.one).y
      Vec3.one.scale(1f-t) + Vec3(0.5f, 0.7f, 1f).scale(t)
  }

  def traceRay(objs: Objs, width: Int, height: Int, camera: Camera, j: Int, i: Int): Color =
    rayColor(objs,
      getRay(camera,
        i.toDouble / width.toDouble,
        j.toDouble / height.toDouble), 0)

  def colorToPixel(c: Color): Image.Pixel =
    ((255.99f * c.x).toByte, (255.99f * c.y).toByte, (255.99f * c.z).toByte)


  def render(objs: Objs, width: Int, height: Int, camera: Camera): Image =
    Image(width, height, (j, i) =>
        colorToPixel(traceRay(objs, width, height, camera, j, i)))

  def time[R](text: String, reps: Int, block: => R): R = {
    val timingHack: Seq[(Double, R)] = (1 to reps).map { _ =>
      val t0 = System.nanoTime()
      val result = block    // call-by-name
      val t1 = System.nanoTime()
      ((t1 - t0)/1000000.0, result)
    }
    val timings = timingHack.map(_._1)
    println(s"$text: ${timings.sum / timings.length}ms (average over $reps repetitions)")
    // First result
    timingHack.head._2
  }
}
