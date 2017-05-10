import io.StdIn._

/* Ray definition: r(t) = r0 + t * direction, t >= 0
  intersection by solving for the t that satisfies a condition
  sphere: (r(t)-n)*(r(t)-n) = radius^2
  plane: r(t)* n = d
  can become quadratic equation, solve for smaller value of t
*/

class Point3D(val x:Int, val y:Int, val z:Int){
  def times(num:Int)={
    new Point3D(x*num, y*num, z*num)
  }

  def add(point:Point3D)={
    new Point3D(x+point.x, y+point.y, z+point.z)
  }
}
class Ray(startPoint:Point3D, direction:Point3D){
  def ray(t:Int)={
    startPoint.add(direction.times(t))
  }
}
val point = new Point3D(0,0,0)
val dir = new Point3D(2,3,1)
val r = new Ray(point, dir)

for(n <- Range(0,10)){
  var temp:Point3D = r.ray(n)
  println(temp.x, temp.y, temp.z)
}
