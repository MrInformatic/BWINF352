package kreis.code.analysis.processing.originalimage

import java.awt.image.BufferedImage

import kreis.code.analysis.processing.{Pipeline, Processor}

/**
  * Wandelt ein BufferedImage in ein Bild um
  */
class OriginalImageProcessor extends Processor[BufferedImage,OriginalImage]{
  /**
    * Der Name der Station
    */
  lazy val name = "Original Image"

  /**
    * Wandelt ein BufferedImage in ein Bild um
    * @param input Das BufferedImage
    * @return Das Bild
    */
  override def produce(input:BufferedImage):OriginalImage = OriginalImage(input)
}
