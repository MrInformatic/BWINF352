package kreis.code.analysis.processing.grayscaleimage

import java.awt.Color
import java.awt.image.BufferedImage

import kreis.code.analysis.processing.Result
import kreis.code.analysis.processing.originalimage.OriginalImage
import kreis.code.utils.Vector2I

import scala.collection.mutable.LazyBuilder

/**
  * Ein Schwarz-Weis-Bild
  * @param width Die Breite des Bildes
  * @param height Die Höhe des Bildes
  * @param originalImage Das Orginalbild
  */
abstract class GrayscaleImage(val width:Int,val height:Int,val originalImage: OriginalImage) extends Result{
  /**
    * Gibt die Farbe eines Pixels zurück
    * @param position Die Position des Pixels
    * @return Die Farbe des Pixels
    */
  def apply(position:Vector2I): (Int)

  /**
    * Gibt die Farben aller Pixel in einer 2-dimensionalen Array aus
    * @return Die 2-dimensionenale Array
    */
  def apply:Array[Array[Int]] =
    {println("Test");Array.tabulate(width,height)(apply(_)(_))}

  /**
    * Das Bild als BufferedImage
    */
  lazy val debugImage: BufferedImage = {
    val result = new BufferedImage(width,height,BufferedImage.TYPE_BYTE_GRAY)

    for(x <- 0 until width;y <- 0 until height){
      result.setRGB(x,y,apply(x,y)*0x00010101)
    }
    result
  }
}

object GrayscaleImage {
  /**
    * Erzeugt ein Schwarz-Weiß-Bild aus einer 2-dimensionalen Array, die die Farben aller Pixel enthält
    * @param width Die Breite des Bildes
    * @param height Die Höhe des Bildes
    * @param data Die 2-dimensionale Array
    * @param originalImage Das Orginalbild
    * @return Das Schwarz-Weiß-Bild
    */
  def apply(width: Int,height: Int,data: Array[Array[Int]],originalImage: OriginalImage) = new GrayscaleImage(width,height,originalImage) {
    /**
      * Gibt die Farbe eines Pixels zurück
      * @param position Die Position des Pixels
      * @return Die Farbe des Pixels
      */
    override def apply(position: Vector2I): Int = data(position.x)(position.y)

    /**
      * Gibt die Farben aller Pixel in einer 2-dimensionalen Array aus
      * @return Die 2-dimensionenale Array
      */
    override def apply: Array[Array[Int]] = data
  }
}
