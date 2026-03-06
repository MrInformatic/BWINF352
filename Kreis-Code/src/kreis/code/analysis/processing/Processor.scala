package kreis.code.analysis.processing

import scala.collection.mutable.ListBuffer

/**
  * Ein Station der Pipeline
  * @tparam I Der Eingabetyp
  * @tparam O Der Ausgabetyp
  */
abstract class Processor[I, +O <: Result] extends Pipeline[I,O]{
  /**
    * Der Name der Station
    */
  val name: String

  /**
    * Der Name der Station
    */
  lazy val segNames: ListBuffer[String] = ListBuffer(name)

  /**
    * Ergebnis der Station
    * @param input Die Eingabe
    * @return Das Ergebnis der Station
    */
  override def produceAllResults(input:I):ListBuffer[Result] = try ListBuffer(produce(input)) catch{
    case e: Exception => {
      e.printStackTrace()
      ListBuffer.empty
    }
  }
}
