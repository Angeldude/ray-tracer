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
}

val f = new Point3D(2,3,1)
val newPoint = f `+` (Vector(3,2,4).map(i => i.toDouble * 3))
println(newPoint.x,newPoint.y,newPoint.z)
