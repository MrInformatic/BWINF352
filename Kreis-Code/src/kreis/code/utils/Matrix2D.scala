package kreis.code.utils

/**
  * Eine 2x2 Matrix aus Doubles
  */
class Matrix2D(val x1:Double,val y1:Double,val x2:Double,val y2:Double) {
  def this(){
    this(1,0,0,1)
  }

  /**
    * Multipliziert einen Vektor mit der Matrix
    * @param vector Der Vektor
    * @return Der neue Vektor
    */
  def *(vector: Vector2D):Vector2D = new Vector2D(vector.x*x1+vector.y*x2,vector.x*y1+vector.y*y2)
}

object Matrix2D {
  /**
    * Erstellt eine neue 2x2 Matrix als Identity-Matrix
    * @return Die Identity-Matrix
    */
  def apply() = new Matrix2D

  /**
    * Erstellt eine neue Matrix
    * @param x1 Das Feld oben links
    * @param y1 Das Feld open rechts
    * @param x2 Das Feld unten links
    * @param y2 Das Feld unten rechts
    * @return Die Matrix
    */
  def apply(x1:Double,y1:Double,x2:Double,y2:Double) = new Matrix2D(x1,y1,x2,y2)

  /**
    * Erstellt eine Matrix
    * @param xAxis Ein Vektor, der eine Einheit in die X-Richtung zeigt
    * @param yAxis Ein Vektor, der eine Einheit in die Y-Richtung zeigt
    * @return Die Matrix
    */
  def apply(xAxis:Vector2D, yAxis:Vector2D) = new Matrix2D(xAxis.x,xAxis.y,yAxis.x,yAxis.y)
}
