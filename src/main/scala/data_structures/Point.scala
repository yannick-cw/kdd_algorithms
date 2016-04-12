package data_structures

/**
  * A two dimensional point
  */
case class Point(x: Double, y: Double) extends Ordered[Point] {

  def compare(that: Point): Int = {
    this - that match {
      case point if point.y != 0 => point.y.toInt
      case point => point.x.toInt
    }
  }

  def -(that: Point): Point = {
    Point(x - that.x, y - that.y)
  }

  def +(that: Point): Point = {
    Point(x + that.x, y + that.y)
  }

  def abs(): Point = {
    Point(scala.math.abs(x), scala.math.abs(y))
  }

  def *(that: Point): Double = {
    x * that.x + y * that.y
  }

  def norm: Double = {
    scala.math.sqrt(x * x + y * y)
  }

  def distManhatten(that: Point): Double = {
    (x - that.x).abs + (y - that.y).abs
  }

  def distEuklid(that: Point): Double = {
    import scala.math._
    sqrt(pow(x - that.x, 2) + pow(y - that.y, 2))
  }

  override def toString: String = s"($x,$y)"
}
