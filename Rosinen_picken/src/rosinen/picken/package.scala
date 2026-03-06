package rosinen

package object picken {

  /**
    * Ein Typ, um Traversable um bestimmte Funktionen zu erweitern
    * @param a Das Traversable
    * @tparam A Typ der im Traversable gespeicherten Daten
    * @tparam T Typ des Traversables
    */
  class Tappable[A,T[X] <: Traversable[X]](a: T[A]) {
    /**
      * Funktion, die eine Operation auf alle Elemente eines Traversables ausführt, ohne diese dabei zu verändern
      * @param action Die Operation
      * @tparam U Void
      * @return Das Traversable
      */
    def tap[U](action: (A) => U): T[A] = {
      a.map(i => {
        action(i)
        i
      }).asInstanceOf[T[A]];
    }
  }

  /**
    * Wandelt ein Traversable in ein Tapable um
    * @param a Das Traversable
    * @tparam A Typ der im Travasable gespeicherten Daten
    * @tparam T Typ des Traversables
    * @return Das Tapable
    */
  implicit def Traversable2Tappable[A,T[X] <: Traversable[X]](a: T[A]): Tappable[A,T] = new Tappable[A,T](a)
}
