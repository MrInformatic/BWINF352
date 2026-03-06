package kreis.code.analysis.encoder

import scala.collection.mutable

/**
  * Ein Binärbaum zum Umwandeln von Binärcodes in Zeichen
  * @param dictionary Eine Sammlung, die alle Binärcodes und ihre resultierenden Zeichen enthält
  * @tparam T Die Art der Sammlung
  */
class BinaryToAsciiConverter[T[X] <: Traversable[X]](dictionary: T[(mutable.BitSet,Char)]){
  /**
    * Der Rückgabewert, wenn keine Lösung gefunden werden konnte
    */
  val default = 255.toChar;
  /**
    * Der Baum, der zum Umwandeln genutzt wird. Es gibt zwei Möglichkeiten pro Stelle im Binärcode. Insgesammt gibt es
    * 16 Stellen
    */
  private val encoderTree = new DecoderTree[Char](2,16,default);

  dictionary.foreach(element => encoderTree.insertValue(depth => if(element._1(depth)) 1 else 0,element._2))

  /**
    * Wandelt einen Binärcode in ein Zeichen um
    * @param bitSet Der Binärcode
    * @return Das Zeichen
    */
  def convert(bitSet: mutable.BitSet): Char ={
    encoderTree.getValue(n => if(bitSet(n)) 1 else 0)
  }

  /**
    * Rotiert einen  Binärcode
    * @param bitSet Der Binärcode
    * @param ammount Um wieviele Stellen rotiert werden soll
    * @return Der rotierte Binärcode
    */
  private def rotate(bitSet: mutable.BitSet,ammount:Int):mutable.BitSet = bitSet.map(i => (i+ammount)%16);

}
