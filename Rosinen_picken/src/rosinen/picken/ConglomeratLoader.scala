package rosinen.picken

import java.io.{BufferedReader, File, FileReader}

import scala.collection.mutable.ListBuffer


/**
  * Objekt zum laden von Konglomeraten
  */
object ConglomeratLoader{
  /**
    * Lädt ein Konglomerat aus einer Datei
    * @param file Die Datei
    * @return Das Konglomerat
    */
  def load(file: File) : Conglomerat = {
    println(file)
    val reader:BufferedReader = new BufferedReader(new FileReader(file));

    val companyCount:Int = getStream(reader)
      .take(1)
      .mkString.toInt;

    val companys:Array[Company] = getStream(reader)
      .take(companyCount)
      .map(_.split(" "))
      .map(s => new Company(s(0).toInt,s(1).toDouble))
      .toArray;

    val edges = getStream(reader)
      .map(_.split(" ").map(s => companys(s.toInt)))
      .map(a => (a(0),a(1)));

    edges
      .foldLeft((0 until companyCount).map(i => (companys(i),new ListBuffer[Company]())).toArray)((result,edge) => {
        result(edge._1.id)._2 += edge._2
        result
      })
      .foreach(childs => childs._1.childs = childs._2.toArray);

    edges
      .foldLeft((0 until companyCount).map(i => (companys(i),new ListBuffer[Company]())).toArray)((result,edge) => {
        result(edge._2.id)._2 += edge._1
        result
      })
      .foreach(parents => parents._1.parents = parents._2.toArray);

    return new SuperConglomerat(companys);
  }

  /**
    * Gibt einen Stream der Zeilen des Readers, ausgeschlossen der Kommentierten Zeilen, zurück
    * @param reader Der Reader
    * @return Der Stream
    */
  private def getStream(reader:BufferedReader) = {
    Stream.continually(reader.readLine())
      .takeWhile(_!=null)
      .filterNot(_.charAt(0)=='#')
  }
}
