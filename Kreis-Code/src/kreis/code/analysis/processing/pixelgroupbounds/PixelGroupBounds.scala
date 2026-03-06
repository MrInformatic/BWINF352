package kreis.code.analysis.processing.pixelgroupbounds

import kreis.code.analysis.processing.analysableimage.AnalyzableImageSet
import kreis.code.utils.Vector2I

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable

/**
  * Die Boundingbox einer Figur
  * @param x Die X-Position der Figur
  * @param y Die y-Position der Figur
  * @param width Die Breite der Figur
  * @param height Die Höhe der Figur
  */
class PixelGroupBounds(val x:Int, val y:Int, val width:Int, val height:Int) {
  /**
    * Gibt die Position der Figur zurück
    * @return Die Position
    */
  def orgin = Vector2I(x, y)

  /**
    * Gibt die Größe der Figur zurück
    * @return Die Größe
    */
  def size = Vector2I(width, height)

  /**
    * Gibt den Mittelpunkt der Figur zurück
    * @return Der Mittelpunkt
    */
  def center = orgin+(size/2)
}
