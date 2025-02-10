package Zad4

class Cylinder(private var height: Double = 1.0) extends Circle {
  def getHeight(): Double = height

  def setHeight(new_height: Double): this.type = {
    height = new_height
    this
  }


  override def toString: String = {
    s"${ super.toString }, H: $height"
  }
}
