package kreis.code.analysis.processing.analysableimage

import java.awt.image.BufferedImage

import kreis.code.analysis.processing.originalimage.OriginalImage
import kreis.code.utils.Vector2I

import scala.collection.mutable

/**
  * Eine Implementierung des Zweifarben-Bildes, welche ein Set nutzt
  * @param width0 Die Breite des Bildes
  * @param height0 Die Höhe des Bildes
  * @param data Das Set, in dem die Daten gespeichert werden sollen
  * @param originalImage0 Das Orginalbild
  * @tparam S Die Art des Sets
  */
class AnalyzableImageSet[S[X] <: mutable.Set[X]](width0:Int,height0:Int, val data:S[Int],val originalImage0: OriginalImage) extends AnalysableImage(width0,height0,originalImage0){
  /**
    * Gibt zurück, ob sich eine Position noch im Bild befindet
    * @param position Die Position
    * @return Das Ergebnis
    */
  private def isPositionInImage(position:Vector2I): Boolean =
    position.x>=0&&position.x<width&&position.y>=0&&position.y<height

  /**
    * Gibt die X-Komponente einer Position, die durch eine Ganzzahl repräsentiert wird, zurück
    * @param position Die Position
    * @return Die X-Komponente
    */
  private def getXComponenten(position:Int):Int = position%width

  /**
    * Gibt die Y-Komponente einer Position, die durch eine Ganzzahl repräsentiert wird, zurück
    * @param position Die Position
    * @return Die Y-Komponente
    */
  private def getYComponenten(position:Int):Int = Math.floorDiv(position,width)

  /**
    * Wandelt eine Position in eine Position, die durch einen Ganzzahl repräsentiert wird, um
    * @param position Die Position
    * @return Die Ganzahl-Position
    */
  private implicit def positionToInt(position:Vector2I):Int = position.x + position.y * width

  /**
    * Wandelt eine Position, die durch einen Ganzzahl repräsentiert wird, in eine normale Position um
    * @param position Die Ganzahl-Position
    * @return Die Position
    */
  private implicit def intToPosition(position:Int):Vector2I = (getXComponenten(position),getYComponenten(position))

  /**
    * Gibt die Farbe des Pixel zurück
    * @param position Die Position des Pixels
    * @return Die Farbe (true = weiß;false = schwarz)
    */
  override def get(position:Vector2I): Boolean =
    if(isPositionInImage(position))
      data(position)
    else
      false

  /**
    * Setzt die Farbe eines Pixels
    * @param position Die Position des Pixels
    * @param value Die neue Farbe (true = weiß;false = schwarz)
    */
  override def set(position:Vector2I, value: Boolean): Unit =
    if(isPositionInImage(position))
      data(position) = value
}

object AnalyzableImageSet {
  /**
    * Erzeugt ein Zweifarben-Bild, welches die BitSet-Implementierung nutzt
    * @param width Die Breite des Bildes
    * @param height Die Höhe des Bildes
    * @param originalImage Das Orginalbild
    * @return Das Zweifarben-Bild
    */
  def getBitSetImpl(width:Int,height:Int,originalImage: OriginalImage) =
    new AnalyzableImageSet(width,height,new mutable.BitSet(width*height),originalImage)
}
