package kreis.code.analysis.processing.imagefilter

import java.awt.image.BufferedImage

import kreis.code.analysis.processing.Processor
import kreis.code.analysis.processing.grayscaleimage.GrayscaleImage
import kreis.code.analysis.processing.originalimage.OriginalImage

/**
  * Erzeugt ein Bild mit gestrecktem Dynamikumfang aus einem Anderen
  */
class NormalizeProcessor() extends Processor[GrayscaleImage,GrayscaleImage]{
  /**
    * Der Name der Station
    */
  lazy val name: String = "Normalized Image"

  /**
    * Erzeugt ein Bild mit gestrecktem Dynamikumfang aus einem Anderen
    * @param input Das Bild
    * @return Das Bild mit gestrecktem Dynamikumfang
    */
  override def produce(input: GrayscaleImage): GrayscaleImage = {
    val width = input.width
    val height = input.height

    val data = input.apply
    val min = data.map(_.min).min
    val max = data.map(_.max).max

    GrayscaleImage(width,height,data.map(_.map(i => ((i-min).toDouble/(max-min).toDouble*255).toInt)),input.originalImage)
  }
}
