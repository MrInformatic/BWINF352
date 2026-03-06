package kreis.code.analysis.processing.pixelgroupbounds

import kreis.code.analysis.processing.Processor

import scala.collection.mutable.ListBuffer

/**
  * Findet die Boundingboxes, die in einer anderen Boundingbox liegen
  */
class StackedPixelGroupProcessor extends Processor[PixelGroupBoundsImage,PixelGroupBoundsImage]{
  /**
    * der Name der Station
    */
  lazy val name: String = "Stacked Pixel Group Image"

  /**
    * Findet die Boundingboxes, die in einer anderen Boundingbox liegen
    * @param input Die Boundingboxes
    * @return Die Boundingboxes, die in einer anderen Boundingbox liegen
    */
  override def produce(input: PixelGroupBoundsImage):PixelGroupBoundsImage = {
    val width = input.width
    val height = input.height
    val pixelGroups = input.pixelGroupsBounds.toStream

    new PixelGroupBoundsImage(width,height,pixelGroups.filter(a => pixelGroups.exists(b => isAStackedInsideB(a,b))).toArray,input.analysableImage,input.originalImage)
  }

  /**
    * Gibt zurück, ob b in a liegt
    * @param a Die erste Boundingbox
    * @param b Die zweite Boundingbox
    * @return Das Ergebnis
    */
  private def isAStackedInsideB(a:PixelGroupBounds, b:PixelGroupBounds): Boolean =
    a.x>b.x && a.y>b.y && a.x+a.width<b.x+b.width && a.y+a.height<b.y+b.height
}
