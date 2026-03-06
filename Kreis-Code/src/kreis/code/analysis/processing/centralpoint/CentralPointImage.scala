package kreis.code.analysis.processing.centralpoint

import java.awt.{BasicStroke, Color, Graphics2D}
import java.awt.image.BufferedImage

import kreis.code.analysis.processing.Result
import kreis.code.analysis.processing.analysableimage.AnalysableImage
import kreis.code.analysis.processing.originalimage.OriginalImage
import kreis.code.utils.Vector2I

/**
  * Ein Bild, welches die Mittelpunkte der Kreiscodes speichert
  * @param width Die Breite des Bildes
  * @param height Die Höhe des Bildes
  * @param centralPoints Die Mittelpunkte des Kreiscodes
  * @param analysableImage Die Informationen des Bildes
  * @param originalImage Das Orginalbild
  */
class CentralPointImage(val width: Int, val height: Int, val centralPoints: Array[Vector2I], val analysableImage: AnalysableImage, val originalImage: OriginalImage) extends Result{
  /**
    * Das Bild als BufferedImage mit den markierten Mittelpunkten der Kreiscodes
    */
  override val debugImage: BufferedImage = {
    val oldimage = analysableImage.debugImage
    val image = new BufferedImage(width,height,BufferedImage.TYPE_INT_RGB)
    oldimage.copyData(image.getRaster)

    val graphics = image.getGraphics.asInstanceOf[Graphics2D]

    graphics.setStroke(new BasicStroke(5))
    graphics.setColor(Color.RED)
    for(point <- centralPoints){
      graphics.drawLine(point.x-20,point.y-20,point.x+20,point.y+20)
      graphics.drawLine(point.x-20,point.y+20,point.x+20,point.y-20)
    }
    image
  }
}
