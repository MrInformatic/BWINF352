package kreis.code.analysis.encoder

import java.io.{BufferedReader, File, FileReader}

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable

object BinaryToAsciiConverterLoader {
  /**
    * Erstellt einen Binärbaum, der Kreiscode-Binärcode in Zeichen umwandeln kann
    * @param file Die Datei, in der steht, welcher Binärcode in welches Zeichen umgewandelt wird
    * @return Den Binärbaum
    */
  def load(file: File) = {
    val reader: BufferedReader = new BufferedReader(new FileReader(file))

    new BinaryToAsciiConverter(getStream(reader)
      .map(_.split(" "))
      .map(a => (a(0).split("")
        .reverse
        .zipWithIndex
        .foldLeft(new mutable.BitSet(16))((b,c) => {if(c._1=="1") b += c._2;b}),
        a(1).toInt.toChar))
      .flatMap(a => (0 until 16).map(b => (rotate(a._1,b),a._2))))
  }

  /**
    * Rotiert einen Binärcode
    * @param bitSet Der Binärcode
    * @param ammount Um wieviele Stellen rotiert werden soll
    * @return Der rotierte Binärcode
    */
  private def rotate(bitSet: mutable.BitSet,ammount:Int):mutable.BitSet = bitSet.map(i => (i+ammount)%16);

  /**
    * Gibt einen Stream aus, der alle Zeilen des BufferedReaders enthält
    * @param reader Der BufferedReader
    * @return Der Stream
    */
  private def getStream(reader:BufferedReader) = {
    Stream.continually(reader.readLine())
      .takeWhile(_!=null)
  }
}
