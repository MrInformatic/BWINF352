package kreis.code.analysis.processing.analysableimage

import java.awt.Color
import java.awt.image.BufferedImage

import kreis.code.analysis.processing.Result
import kreis.code.analysis.processing.originalimage.OriginalImage
import kreis.code.utils.Vector2I

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable

/**
  * Ein Zweifarben-Bild
  * @param width Die Breite des Bildes
  * @param height Die Höhe des Bildes
  * @param originalImage Das Orginalbild
  */
abstract class AnalysableImage(val width:Int, val height:Int,val originalImage: OriginalImage) extends Result{
  /**
    * Färbt einen Pixel weiß
    * @param position Die Position des Pixels
    */
  def +=(position:Vector2I): Unit =
    this.add(position)

  /**
    * Färbt einen Pixel weiß
    * @param x Die X-Position des Pixels
    * @param y Die Y-Position des Pixels
    */
  def +=(x:Int,y:Int): Unit =
    this.add((x,y))

  /**
    * Färbt einen Pixel weiß
    * @param position Die Position des Pixels
    */
  def add(position:Vector2I): Unit =
    this.set(position,true)

  /**
    * Färbt einen Pixel weiß
    * @param x Die X-Position des Pixels
    * @param y Die Y-Position des Pixels
    */
  def add(x:Int,y:Int): Unit =
    this.add((x,y))

  /**
    * Färbt einen Pixel schwarz
    * @param position Die Position des Pixels
    */
  def -=(position:Vector2I): Unit =
    this.remove(position)

  /**
    * Färbt einen Pixel schwarz
    * @param x Die X-Position des Pixels
    * @param y Die Y-Position des Pixels
    */
  def -=(x:Int,y:Int): Unit =
    this.remove((x,y))

  /**
    * Färbt einen Pixel schwarz
    * @param position Die Position des Pixels
    */
  def remove(position:Vector2I): Unit =
    this.set(position,false)

  /**
    * Färbt einen Pixel schwarz
    * @param x Die X-Position des Pixels
    * @param y Die Y-Position des Pixels
    */
  def remove(x:Int,y:Int): Unit =
    this.remove((x,y))

  /**
    * Gibt die Farbe des Pixel zurück
    * @param position Die Position des Pixels
    * @return Die Farbe (true = weiß;false = schwarz)
    */
  def apply(position:Vector2I):Boolean =
    this.get(position)

  /**
    * Gibt die Farbe des Pixel zurück
    * @param x Die X-Position des Pixels
    * @param y Die Y-Position des Pixels
    * @return Die Farbe (true = weiß;false = schwarz)
    */
  def apply(x:Int,y:Int):Boolean =
    this.get((x,y))

  /**
    * Gibt die Farbe des Pixel zurück
    * @param position Die Position des Pixels
    * @return Die Farbe (true = weiß;false = schwarz)
    */
  def get(position:Vector2I): Boolean

  /**
    * Gibt die Farbe des Pixel zurück
    * @param x Die X-Position des Pixels
    * @param y Die Y-Position des Pixels
    * @return Die Farbe (true = weiß;false = schwarz)
    */
  def get(x:Int,y:Int): Boolean =
    this.get((x,y))

  /**
    * Setzt die Farbe eines Pixels
    * @param position Die Position des Pixels
    * @param value Die neue Farbe (true = weiß;false = schwarz)
    */
  def update(position:Vector2I,value:Boolean): Unit =
    this.set(position,value)

  /**
    * Setzt die Farbe eines Pixels
    * @param x Die X-Position des Pixels
    * @param y Die Y-Position des Pixels
    * @param value Die neue Farbe (true = weiß;false = schwarz)
    */
  def update(x:Int,y:Int,value:Boolean): Unit =
    this.set((x,y),value)

  /**
    * Setzt die Farbe eines Pixels
    * @param position Die Position des Pixels
    * @param value Die neue Farbe (true = weiß;false = schwarz)
    */
  def set(position:Vector2I, value:Boolean): Unit

  /**
    * Setzt die Farbe eines Pixels
    * @param x Die X-Position des Pixels
    * @param y Die Y-Position des Pixels
    * @param value Die neue Farbe (true = weiß;false = schwarz)
    */
  def set(x:Int,y:Int,value:Boolean): Unit =
    set((x,y),value)

  /**
    * Das Bild als BufferedImage
    */
  lazy val debugImage ={
    val result = new BufferedImage(width,height,BufferedImage.TYPE_INT_RGB)
    for(x <- 0 until width;y <- 0 until height)
      if(apply(x,y))
        result.setRGB(x,y,0x00FFFFFF)
    result
  }
}
