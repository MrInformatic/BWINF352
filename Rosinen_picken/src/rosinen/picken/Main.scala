package rosinen.picken

import java.io.File

/**
  * Hauptklasse
  */
object Main {
  def main(args : Array[String]) : Unit = {
    args.grouped(2).foreach(a => ResultWriter.write(new File(a(1)),ConglomeratLoader.load(new File(a(0)))));
  }
}

