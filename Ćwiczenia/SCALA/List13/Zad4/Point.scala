package Zad4

class Point(private var x_coord: Double = 1.0, private var y_coord: Double = 1.0) {
  def getX(): Double = x_coord
  def getY(): Double = y_coord
  
  def setX(new_x: Double): this.type = {
    x_coord = new_x
    this
  }
  
  def setY(new_y: Double): this.type = {
    y_coord = new_y
    this
  }


  override def toString: String = {
    s"X: $x_coord, Y: $y_coord "
  }
}
