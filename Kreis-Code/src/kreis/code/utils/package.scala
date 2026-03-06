package kreis.code

package object utils {
  /**
    * Wandelt ein Tupel in einen Vektor um
    * @param tuple Das Tupel
    * @return Der Vektor
    */
  implicit def tupleToVector2D(tuple:(Double,Double)):Vector2D =
    new Vector2D(tuple._1,tuple._2)

  /**
    * Wandelt ein Tupel in eine Matrix um
    * @param tuple Das Tupel
    * @return Die Matrix
    */
  implicit def tupleToMatrix2D(tuple:((Double,Double),(Double,Double))):Matrix2D =
    new Matrix2D(tuple._1._1,tuple._1._2,tuple._2._1,tuple._2._2)

  /**
    * Wandelt ein Tupel in einen Vektor um
    * @param tuple Das Tupel
    * @return Der Vektor
    */
  implicit def tupleToVector2I(tuple:(Int,Int)):Vector2I =
    new Vector2I(tuple._1,tuple._2)

  /**
    * Wandelt einen Double-Vektor in einen Int-Vektor um
    * @param vector2D Der Double-Vektor
    * @return Der Int-Vektor
    */
  implicit def vector2Dto2I(vector2D: Vector2D):Vector2I =
    vector2D()

  /**
    * Wandelt einen Int-Vektor in einen Double-Vektor um
    * @param vector2I Der Int-Vektor
    * @return Der Double-Vektor
    */
  implicit def vector2Ito2D(vector2I: Vector2I):Vector2D =
    vector2I()
}
