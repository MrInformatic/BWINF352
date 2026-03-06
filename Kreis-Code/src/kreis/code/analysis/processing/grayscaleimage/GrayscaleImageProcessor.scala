package kreis.code.analysis.processing.grayscaleimage

import kreis.code.analysis.processing.Processor
import kreis.code.analysis.processing.originalimage.OriginalImage

/**
  * Wandelt ein Bild in ein Schwarz-Weiß-Bild um
  */
class GrayscaleImageProcessor extends Processor[OriginalImage,GrayscaleImage]{
  /**
    * Der Name der Station
    */
  lazy val name: String = "Grayscale Image"

  /**
    * Wandelt ein Bild in ein Schwarz-Weiß-Bild um
    * @param input Das Bild
    * @return Das Schwarz-Weiß-Bild
    */
  override def produce(input: OriginalImage): GrayscaleImage = {
    val width = input.width
    val height = input.height
    val result = Array.ofDim[Int](width,height)

    for(x <- 0 until width;y <- 0 until height){
      val data = input(x,y)
      result(x)(y) = Math.round((data._1+data._2+data._3)/3.0).toInt
    }

    GrayscaleImage(width,height,result,input)
  }
}
