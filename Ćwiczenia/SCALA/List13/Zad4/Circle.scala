package Zad4

class Circle(private var radius: Double = 1.0) extends Point {
  def getRadius(): Double = radius
  
  def setRadius(new_radius: Double): this.type = {
    radius = new_radius
    this
  }


  override def toString: String = {
    s"${ super.toString }, R: $radius "
  }
}
