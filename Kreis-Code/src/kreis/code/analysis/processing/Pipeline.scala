package kreis.code.analysis.processing

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

/**
  * Eine Pipeline
  * @tparam I Der Eingabetyp
  * @tparam O Der Ausgabetyp
  */
abstract class Pipeline[I, +O <: Result] {
  /**
    * Die Namen der Stationen der Pipeline
    */
  val segNames: ListBuffer[String]

  /**
    * Gibt nur das Ergebnis der letzten Station zurück
    * @param input Die Eingabe
    * @return Das Ergebnis der letzten Station
    */
  def produce(input:I): O

  /**
    * Gibt das Ergebnis aller Station zurück
    * @param input Die Eingabe
    * @return Das Ergebnis aller Station
    */
  def produceAllResults(input:I):ListBuffer[Result]

  /**
    * Verbindet zwei Pipelines
    * @param seg Die weiterführende Pipeline
    * @tparam X Der Ausgabetyp der weiterführenden Pipeline
    * @tparam Y Der Eingabetyp der weiterführenden Pipeline
    * @return Die neue Pipeline
    */
  def -->[X <: Result,Y >: O](seg:Pipeline[Y, X]):Pipeline[I, X] = {
    val thisInstance = this
    new Pipeline[I, X] {
      /**
        * Die Namen der Stationen der neuen Pipeline
        */
      lazy val segNames = {thisInstance.segNames ++= seg.segNames; thisInstance.segNames}

      /**
        * Gibt nur das Ergebnis der letzten Station zurück
        * @param input Die Eingabe
        * @return Das Ergebnis der letzten Station
        */
      override def produce(input: I): X = seg.produce(thisInstance.produce(input))

      /**
        * Gibt das Ergebnis aller Station zurück
        * @param input Die Eingabe
        * @return Das Ergebnis aller Station
        */
      override def produceAllResults(input: I): ListBuffer[Result] ={
        val result1 = thisInstance.produceAllResults(input)
        result1 ++= seg.produceAllResults(result1.last.asInstanceOf[Y])
        result1
      }
    }
  }
}
