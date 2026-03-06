package kreis.code.gui

import java.awt.image.{BufferedImage, ColorModel, WritableRaster}
import java.awt.{Color, Dimension, Graphics}
import javax.swing.JPanel

import kreis.code.analysis.processing.analysableimage.AnalyzableImageSet
import sun.awt.image.DataBufferNative

import scala.collection.mutable

/**
  * Zeigt ein Bild an
  */
class ImageView extends JPanel{
  /**
    * Das Bild
    */
  private var image0:BufferedImage = null;

  /**
    * Gibt das Bild zurück
    * @return Das Bild
    */
  def image:BufferedImage = image0;

  /**
    * Setzt das Bild
    * @param image Das Bild
    */
  def image_=(image: BufferedImage):Unit = {
    image0 = image
    setSize(new Dimension(image0.getWidth,image0.getHeight))
    setPreferredSize(new Dimension(image0.getWidth,image0.getHeight))
  }

  /**
    * Zeichnet das Bild
    * @param graphics Das Canvas
    */
  override def paint(graphics: Graphics): Unit = {
    super.paint(graphics)
    graphics.drawImage(image0,0,0,null)
  }
}
