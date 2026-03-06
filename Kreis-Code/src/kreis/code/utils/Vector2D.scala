package kreis.code.utils

/**
  * Ein Vektor aus 2 Doubles (Double-Vektor)
  * @param x Die X-Koordinate des Vektors
  * @param y Die Y-Koordinate des Vektors
  */
class Vector2D(val x:Double,val y:Double) {
  def this() {
    this(0,0)
  }

  /**
    * Addiert diesen und einen anderen Vektor
    * @param otherVector Der andere Vektor
    * @return Der neue Vektor
    */
  def +(otherVector:Vector2D):Vector2D = new Vector2D(this.x+otherVector.x,this.y+otherVector.y)

  /**
    * Subtrahiert diesen und einen anderen Vektor
    * @param otherVector Der andere Vektor
    * @return Der neue Vektor
    */
  def -(otherVector:Vector2D):Vector2D = new Vector2D(this.x-otherVector.x,this.y-otherVector.y)

  /**
    * Bildet das Skalarprodukt aus diesem und einen anderen Vektor
    * @param otherVector Der andere Vektor
    * @return Das Ergebnis
    */
  def *(otherVector:Vector2D):Double = this.x*otherVector.x+this.y*otherVector.y

  /**
    * Skaliert den Vektor um einen Wert
    * @param scalar Der Wert
    * @return Der neue Vektor
    */
  def *(scalar:Double):Vector2D = new Vector2D(this.x*scalar,this.y*scalar)

  /**
    * Skaliert den Vektor um den Kehrwert eines Wertes
    * @param scalar Der Wert
    * @return Der neue Vektor
    */
  def /(scalar:Double):Vector2D = new Vector2D(this.x/scalar,this.y/scalar)

  /**
    * Gibt die Länge des Vektors zurück
    * @return Die Länge des Vektors
    */
  def len = Math.sqrt(x*x+y*y)

  /**
    * Wandelt den Vektor in einen Int-Vektor um
    * @return Der Int-Vektor
    */
  def apply() = new Vector2I(Math.round(x).toInt,Math.round(y).toInt)

  /**
    * Gibt einen Vektor zurück, der genau rechtwinklig zu diesem Vektor steht
    * @return Der rechtwinklige Vektor
    */
  def ortho() = new Vector2D(y,-x)

  /**
    * Wandelt den Vektor in einen String um
    * @return Der String
    */
  override def toString = s"Vector2D($x, $y)"
}

object Vector2D {
  /**
    * Erstellt einen Vektor
    * @param x Die X-Koordinate des Vektors
    * @param y Die Y-Koordinate des Vektors
    * @return Der Vektor
    */
  def apply(x:Double,y:Double) = new Vector2D(x,y)

  /**
    * Erstellt einen Vektor mit den Koordinaten (0,0)
    * @return Der Vektor
    */
  def apply() = new Vector2D

  /**
    * Erstellt einen Vektor aus einem Int-Vektor
    * @param vector2I Der Int-Vektor
    * @return Der neue Vektor
    */
  def apply(vector2I: Vector2I):Vector2D = vector2I()
}
