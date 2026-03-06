package kreis.code.analysis.processing.centralpoint

import kreis.code.analysis.processing.Processor
import kreis.code.analysis.processing.pixelgroupbounds.PixelGroupBoundsImage

/**
  * Findet die Mittelpunkte der Figuren
  */
class CentralPointImageProcessor extends Processor[PixelGroupBoundsImage,CentralPointImage]{
  /**
    * Der Name der Station
    */
  lazy val name: String = "Central Point Image"

  /**
    * Findet die Mittelpunkte der Figuren
    * @param input Die Figuren
    * @return Die Mittelpunkte
    */
  override def produce(input: PixelGroupBoundsImage): CentralPointImage ={
    val width = input.width
    val height = input.height

    new CentralPointImage(width,height,input.pixelGroupsBounds
      .map(pgb => pgb.center),input.analysableImage,input.originalImage)
  }
}
