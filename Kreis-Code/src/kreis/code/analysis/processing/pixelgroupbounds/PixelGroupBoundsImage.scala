package kreis.code.analysis.processing.pixelgroupbounds

import java.awt.{BasicStroke, Color, Graphics2D}
import java.awt.image.BufferedImage

import kreis.code._
import kreis.code.analysis.processing.Result
import kreis.code.analysis.processing.analysableimage.AnalysableImage
import kreis.code.analysis.processing.originalimage.OriginalImage

import scala.collection.mutable

/**
  * Ein Bild, welches die Boundingbox der Figuren speichert
  * @param width Die Breite des Bildes
  * @param height Die Höhe des Bildes
  * @param pixelGroupsBounds Die Boundingboxes der Figuren
  * @param analysableImage Die Informationen des Bildes
  * @param originalImage Das Orginalbild
  */
class PixelGroupBoundsImage(val width:Int, val height:Int, val pixelGroupsBounds:Array[PixelGroupBounds], val analysableImage:AnalysableImage,val originalImage: OriginalImage) extends Result{
  /**
    * Das Bild als BufferedImage mit den markierten Figuren
    */
  override lazy val debugImage: BufferedImage = {
    val oldimage = analysableImage.debugImage
    val image = new BufferedImage(width,height,BufferedImage.TYPE_INT_RGB)
    oldimage.copyData(image.getRaster)

    val graphics = image.getGraphics.asInstanceOf[Graphics2D]

    graphics.setStroke(new BasicStroke(5))
    graphics.setColor(Color.BLUE)
    pixelGroupsBounds.foreach(pg => graphics.drawRect(pg.x,pg.y,pg.width,pg.height))

    image
  }
}
