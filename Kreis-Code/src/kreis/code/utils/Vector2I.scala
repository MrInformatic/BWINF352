package kreis.code.utils

import com.sun.org.apache.xml.internal.utils.IntVector

/**
  * Ein Vektor aus 2 Ints (Int-Vektor)
  * @param x Die X-Koordinate des Vektors
  * @param y Die Y-Koordinate des Vektors
  */
class Vector2I(val x:Int,val y:Int) {
  def this() {
    this(0,0)
  }

  /**
    * Addiert diesen und einen anderen Vektor
    * @param otherVector Der andere Vektor
    * @return Der neue Vektor
    */
  def +(otherVector:Vector2I):Vector2I = new Vector2I(this.x+otherVector.x,this.y+otherVector.y)

  /**
    * Subtrahiert diesen und einen anderen Vektor
    * @param otherVector Der andere Vektor
    * @return Der neue Vektor
    */
  def -(otherVector:Vector2I):Vector2I = new Vector2I(this.x-otherVector.x,this.y-otherVector.y)

  /**
    * Bildet das Skalarprodukt aus diesem und einen anderen Vektor
    * @param otherVector Der andere Vektor
    * @return Das Ergebnis
    */
  def *(otherVector:Vector2I):Double = this.x*otherVector.x+this.y*otherVector.y

  /**
    * Skaliert den Vektor um einen Wert
    * @param scalar Der Wert
    * @return Der neue Vektor
    */
  def *(scalar:Int):Vector2I = new Vector2I(this.x*scalar,this.y*scalar)

  /**
    * Skaliert den Vektor um den Kehrwert eines Wertes
    * @param scalar Der Wert
    * @return Der neue Vektor
    */
  def /(scalar:Int):Vector2I = new Vector2I(this.x/scalar,this.y/scalar)

  /**
    * Gibt die Länge des Vektors zurück
    * @return Die Länge des Vektors
    */
  def len = Math.sqrt(x*x+y*y)

  /**
    * Wandelt den Vektor in einen Double-Vektor um
    * @return Der Double-Vektor
    */
  def apply() = new Vector2D(x,y)

  /**
    * Gibt einen Vektor zurück, der genau rechtwinklig zu diesem Vektor steht
    * @return Der rechtwinklige Vektor
    */
  def ortho() = new Vector2D(y,-x)

  /**
    * Wandelt den Vektor in einen String um
    * @return Der String
    */
  override def toString = s"Vector2I($x, $y)"
}

object Vector2I {
  /**
    * Erstellt einen Vektor
    * @param x Die X-Koordinate des Vektors
    * @param y Die Y-Koordinate des Vektors
    * @return Der Vektor
    */
  def apply(x:Int,y:Int) = new Vector2I(x,y)

  /**
    * Erstellt einen Vektor mit den Koordinaten (0,0)
    * @return Der Vektor
    */
  def apply() = new Vector2I

  /**
    * Erstellt einen Vektor aus einem Double-Vektor
    * @param vector2D Der Double-Vektor
    * @return Der neue Vektor
    */
  def apply(vector2D: Vector2D):Vector2I = vector2D()
}
