import io.StdIn._

class Point3D(val x:Double, val y:Double, val z:Double){
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
  def intersectPlane(r:Ray) = {
    val denominator = r.rD.zip(n).map(i => i._1 * i._2).sum
    val numerator =  d - (r.r0 `*` n)
    if(denominator == 0)
      "Ray is parallel to plane,\nNo intersection"
    else if(numerator/denominator  < 0)
      "t is out of range, no intersection"
    else {
      val t = numerator/denominator
      val point = r.r(t)
        "There is intersection at t = " + t.toString + "\n" +
        "Giving us the point at " + (point.x.floatValue, point.y.floatValue, point.z.floatValue)
      }
   }
}

// sphere: (r(t)-n)*(r(t)-n) = radius^2
class Sphere(center:Point3D, radius:Double){
  def eVector(ray:Ray) = {
    center.sub(ray.r0)
  }

  def intersectSphere(ray:Ray) = {
    val rayVector = Vector(ray.r0.x, ray.r0.y, ray.r0.z)
    val centerVector = Vector(center.x, center.y, center.z)
    val a = ray.rD.map(i => math.pow(i,2)).sum
    val b = ray.rD.zip(eVector(ray)).map(i => 2 * i._1 * i._2).sum
    val c = centerVector.map(i => math.pow(i,2)).sum +
    rayVector.map(i => math.pow(i,2)).sum +
    (-2 * (center `*` rayVector)) - math.pow(radius,2)

    val delta = b*b - (4 * a * c)
    if(delta < 0)
      "There is no intersection."
    else {
      val t0 = (-b - math.sqrt(delta))/2*a
      val t1 = (-b + math.sqrt(delta))/2*a
      val t = Vector(t0,t1).min
      val point = ray.r(t)
      "There is an intersection at t = " + t.toString + "\n" +
      "Giving us the point at " + (point.x.floatValue, point.y.floatValue, point.z.floatValue)
    }
  }
}

val ray = new Ray(new Point3D(1,2,2), Vector(1,1,1))
// val plane = new Plane(Vector(1,2,3), 6)
val sphere = new Sphere(new Point3D(6,7,7), 0.5)
// println(plane.intersectPlane(ray))
println(sphere.intersectSphere(ray))

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
