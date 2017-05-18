import io.StdIn._

class Point3D(val x:Double, val y:Double, val z:Double){
  override def toString = {
    val xF = x.floatValue
    val yF = y.floatValue
    val zF = z.floatValue
    s"($xF, $yF, $zF)"
  }

  def `+`(num: Vector[Double]) = {
    new Point3D(x+num(0), y+num(1), z+num(2))
  }

  def `-`(num:Vector[Double]) = {
    new Point3D(x-num(0), y-num(1), z-num(2))
  }

  def `*`(dot:Vector[Double]) = {
    x*dot(0) + y*dot(1) + z*dot(2)
  }

  def sub(point:Point3D) = {
    Vector(point.x-x, point.y-y, point.z-z)
  }
}

// ray: r0 + rD*t, where t >= 0
class Ray(val r0:Point3D, val rD:Vector[Double]){
  def r(t:Double) = {
    r0 `+` (rD.map(n => n * t))
  }

  def magnitude = {
    val squared = rD.map(i => math.pow(i,2)).sum
    math.sqrt(squared)
  }

  def normalize(t:Double) = {
    val root = magnitude
    val newVector = rD.map(i => i/root)
    r0 `+` (newVector.map(n => n * t))
  }
}

// plane: r(t)* n = d
class Plane(n:Vector[Double], d:Double){
  def tPlane(r:Ray) = {
    val ratio = fraction(r)

    if(ratio._2 == 0)
      0.0
    else
      ratio._1/ratio._2
   }

  private def fraction(r:Ray) = {
     val denominator = r.rD.zip(n).map(i => i._1 * i._2).sum
     val numerator =  d - (r.r0 `*` n)
     (numerator, denominator)
   }
}

// sphere: (r(t)-n)*(r(t)-n) = radius^2
class Sphere(center:Point3D, radius:Double){
  def eVector(ray:Ray) = {
    center.sub(ray.r0)
  }

  def tSphere(ray:Ray) = {
    val calculated = calculate
    val delta = calculated._1
    val a = calculated._2
    val b = calculated._3

    if(delta < 0)
      delta
    else {
      val t0 = quadratic("sub", a, b, delta)
      val t1 = quadratic("add", a, b, delta)
      Vector(t0,t1).min
    }
  }

  private def calculate ={
    val rayVector = Vector(ray.r0.x, ray.r0.y, ray.r0.z)

    val centerVector = Vector(center.x, center.y, center.z)

    val a = ray.rD.map(i => math.pow(i,2)).sum

    val b = ray.rD.zip(eVector(ray)).map(i => 2 * i._1 * i._2).sum

    val c = centerVector.map(i => math.pow(i,2)).sum +
    rayVector.map(i => math.pow(i,2)).sum +
    (-2 * (center `*` rayVector)) - math.pow(radius,2)

    (b*b - (4 * a * c), a, b)
  }

  private def quadratic(operator:String, a:Double, b:Double, del:Double) = {
    if(operator == "add")
      (-b + math.sqrt(del))/(2*a)
    else
      (-b - math.sqrt(del))/(2*a)
  }
}

val ray = new Ray(new Point3D(1,2,2), Vector(1,1,1))
val plane = new Plane(Vector(1,2,3), 6)
val sphere = new Sphere(new Point3D(6,7,7), 1)
println(ray.r(sphere.tSphere(ray)))
println(ray.r(plane.tPlane(ray)))

// println("Enter 3 numbers, new line each, to represent the start point of\na Ray: ")
// val x0 = readDouble()
// val y0 = readDouble()
// val z0 = readDouble()
//
// println("Now enter a direction vector for that ray, also 3 numbers: ")
// val xD = readDouble()
// val yD = readDouble()
// val zD = readDouble()
//
// val ray = new Ray(new Point3D(x0,y0,z0), Vector(xD, yD, zD))
//
// println("Do you want to test for intersection of a sphere or a plane?")
// val check = readLine()
// if(check == "sphere")
//   println("coming soon")
// else if(check == "plane"){
//   println("We'll need 4 numbers for the plane:\n" +
// "the normal vector (3 numbers), and the distance: ")
//   val pX = readDouble()
//   val pY = readDouble()
//   val pZ = readDouble()
//   val pD = readDouble()
//
//   val plane = new Plane(Vector(pX,pY,pZ), pD)
//   println(plane.intersectPlane(ray))
// }
// else
//  println("That wasn't an option. Goodbye.")
