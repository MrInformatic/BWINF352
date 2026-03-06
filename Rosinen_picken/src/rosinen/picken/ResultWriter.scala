package rosinen.picken

import java.io.{BufferedWriter, File, FileWriter}

/**
  * Objekt zum Speichern der Analyseergebnisse
  */
object ResultWriter {
  /**
    * Speichert die Ergebnisse der Analyse des Konglomerats im vorgegebenen Format in einer Datei
    * @param file Die Datei
    * @param conglomerat Das Konglomerat
    */
  def write(file: File, conglomerat: Conglomerat) = {
    val writer: BufferedWriter = new BufferedWriter(new FileWriter(file));

    val raisins = conglomerat.pickRaisins();

    writer.write(raisins.length.toString)
    writer.newLine()
    writer.write(raisins.toStream
      .map(_.value)
      .sum[Double].toString)
    writer.newLine()
    writer.write(raisins.sortBy(_.id).toStream
      .map(_.id)
      .mkString("\n"))
    writer.close()
  }
}
