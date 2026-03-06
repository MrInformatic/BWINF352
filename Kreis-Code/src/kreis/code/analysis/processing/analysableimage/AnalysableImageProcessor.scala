package kreis.code.analysis.processing.analysableimage

import kreis.code.analysis.processing.Processor
import kreis.code.analysis.processing.grayscaleimage.GrayscaleImage
import kreis.code.analysis.processing.originalimage.OriginalImage

import scala.collection.mutable

/**
  * Wandelt ein Schwarz-Weiß-Bild in ein Zweifarben-Bild um
  * @param threshold Die Helligkeit, ab der ein Pixel als weiß gewertet werden soll
  */
class AnalysableImageProcessor(val threshold:Int) extends Processor[GrayscaleImage,AnalysableImage] {
  /**
    * Der Name der Station
    */
  lazy val name: String = "Analysable Image"

  /**
    * Wandelt ein Schwarz-Weiß-Bild in ein Zweifarben-Bild um
    * @param input Das Schwarz-Weiß-Bild
    * @return Das Zweifarben-Bild
    */
  override def produce(input:GrayscaleImage):AnalysableImage = {
    val width = input.width
    val height  = input.height

    val result = AnalyzableImageSet.getBitSetImpl(width,height,input.originalImage)

    for(x <- 0 until width;y <- 0 until height){
      if(input(x,y)<threshold) {
        result += (x,y)
      }
    }

    result
  }
}
