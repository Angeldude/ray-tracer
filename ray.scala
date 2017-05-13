import io.StdIn._

/*
  ray: r0 + rD*t, where t >= 0
  intersection by solving for the t that satisfies a condition
  sphere: (r(t)-n)*(r(t)-n) = radius^2
  plane: r(t)* n = d
  can become quadratic equation, solve for smaller value of t
*/

class Point3D(val x:Double, val y:Double, val z:Double){
  def `+`(num: Vector[Double])={
    new Point3D(x+num(0), y+num(1), z+num(2))
  }

  def `*`(dot:Vector[Double])={
    x*dot(0) + y*dot(1) + z*dot(2)
  }
}

class Ray(r0:Point3D, rD:Vector[Double]){
  def r(t:Double) = {
    r0 `+` (rD.map(n => n * t))
  }

  def normal(t:Double) = {
    val squared = math.pow(rD(0), 2) + math.pow(rD(1), 2) + math.pow(rD(2),2)
    val root = math.sqrt(squared)
    val newVector = Vector(rD(0)/root, rD(1)/root, rD(2)/root)
    r0 `+` (newVector.map(n => n * t))
  }

  def intersectPlane(p:Plane) = {
    val denominator = p.d*p.n(0) + p.d*p.n(1) + p.d*p.n(2)
    val numerator =  p.d - (r0 `*` p.n)
    if(denominator == 0)
      "No intersection"
    else if(numerator/denominator  < 0){
      "t is out of range, no intersection"
    }
    else
      "There is intersection at t = " + (numerator/denominator).toString
   }
}

class Plane(val n:Vector[Double], val d:Double)

val r0 = new Point3D(3,2,5)
val rD = Vector(4,3,2.0)

val ray = new Ray(r0, rD)

val p = new Plane(Vector(1,1,1), 21)
println(ray.intersectPlane(p))
