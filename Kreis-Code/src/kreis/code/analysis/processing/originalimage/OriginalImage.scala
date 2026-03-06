package kreis.code.analysis.processing.originalimage

import java.awt.Color
import java.awt.image.BufferedImage

import kreis.code.analysis.processing.Result

/**
  * Ein Bild
  * @param width Die Breite des Bildes
  * @param height Die Höhe des Bildes
  */
abstract class OriginalImage(val width:Int,val height:Int) extends Result{
  /**
    * Gibt die Farbe eines Pixels zurück
    * @param position Die Position des Pixels
    * @return Die Farbe des Pixels
    */
  def apply(position:(Int,Int)): (Int,Int,Int)

  /**
    * Gibt die Farben aller Pixel in einer 2-dimensionalen Array zurück
    * @return Die 2-dimensionenale Array
    */
  def apply:Array[Array[(Int,Int,Int)]] =
    Array.tabulate(width,height)(apply(_,_))
}

object OriginalImage {
  /**
    * Erzeugt ein Bild aus einem BufferedImage
    * @param image Das BufferedImage
    * @return Das Bild
    */
  def apply(image:BufferedImage) = new OriginalImage(image.getWidth,image.getHeight) {
    /**
      * Gibt die Farbe eines Pixels zurück
      * @param position Die Position des Pixels
      * @return Die Farbe des Pixels
      */
    override def apply(position: (Int, Int)): (Int, Int, Int) = {
      val color = image.getRGB(position._1,position._2)
      return ((color >> 16) & 0x000000FF,(color >> 8) & 0x000000FF,color & 0x000000FF)
    }

    /**
      * Gibt die Farben aller Pixel in einer 2-dimensionalen Array zurück
      * @return Die 2-dimensionenale Array
      */
    override def apply: Array[Array[(Int, Int, Int)]] =
      image.getRGB(0,0,width,height,null,0,width)
        .map(color => ((color >> 16) & 0x000000FF,(color >> 8) & 0x000000FF,color & 0x000000FF))
        .zipWithIndex
        .foldLeft(Array.ofDim[(Int,Int,Int)](width,height))((array,data) =>{
          array(data._2%width)(Math.floorDiv(data._2,width)) = data._1
          array
        })

    /**
      * Das Bild als BufferedImage
      */
    lazy val debugImage = image
  }
}
