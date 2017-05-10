import io.StdIn._

/*
  intersection by solving for the t that satisfies a condition
  sphere: (r(t)-n)*(r(t)-n) = radius^2
  plane: r(t)* n = d
  can become quadratic equation, solve for smaller value of t
*/

class Point3D(val x:Int, val y:Int, val z:Int){
  def `*`(num:Int)={
    new Point3D(x*num, y*num, z*num)
  }

  def `+`(point:Point3D)={
    new Point3D(x+point.x, y+point.y, z+point.z)
  }
}

// Ray definition: r(t) = r0 + (rD * t), t >= 0
class Ray(r0:Point3D, rD:Point3D){
  def r(t:Int)={
    r0 `+` (rD `*` t)
  }
}
val point = new Point3D(0,0,0)
val dir = new Point3D(2,3,1)
val ray = new Ray(point, dir)

for(n <- (0 to 10)){
  var temp:Point3D = ray.r(n)
  println(temp.x, temp.y, temp.z)
}
