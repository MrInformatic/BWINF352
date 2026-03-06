package kreis.code.analysis.processing.imagefilter

import kreis.code.analysis.processing.Processor
import kreis.code.analysis.processing.grayscaleimage.GrayscaleImage
import kreis.code.utils.Vector2I

import scala.collection.parallel.mutable.ParArray

/**
  * Erzeugt ein Bild mit gestrecktem Dynamikumfang aus einem Anderen
  * @param size Die größe der einzelnen Bereiche
  * @param normalizingLimit Der minimale Dynamikumfang, den ein Bereich haben muss, um nicht einheitlich eingefärbt zu
  *                         werden
  */
class LocalNormalizeProcessor(val size:Int,val normalizingLimit:Int) extends Processor[GrayscaleImage,GrayscaleImage]{
  /**
    * Der Name der Station
    */
  lazy val name: String = "Local Normalized Image"

  /**
    * Erzeugt ein Bild mit gestrecktem Dynamikumfang aus einem Anderen
    * @param input Das Bild
    * @return Das Bild mit gestrecktem Dynamikumfang
    */
  override def produce(input: GrayscaleImage): GrayscaleImage = {
    val width = input.width
    val height = input.height

    def isPositionInImage(position:Vector2I): Boolean =
      position.x>=0&&position.x<width&&position.y>=0&&position.y<height

    val result = Array.ofDim[Int](width,height)

    for(bx <- 0 to Math.floorDiv(width,size);by <- 0 to Math.floorDiv(height,size)){
      var min = 255
      var max = 0
      for(x <- bx*size until bx*size+size;y <- by*size until by*size+size){
        if(isPositionInImage(x,y)) {
          min = Math.min(min, input(x,y))
          max = Math.max(max, input(x,y))
        }
      }
      if(max-min > normalizingLimit) {
        for (x <- bx * size until bx * size + size; y <- by * size until by * size + size) {
          if (isPositionInImage(x, y)) {
            result(x)(y) = ((input(x, y) - min).toDouble / (max - min).toDouble * 255).toInt
          }
        }
      }
      else{
        for (x <- bx * size until bx * size + size; y <- by * size until by * size + size) {
          if (isPositionInImage(x, y)) {
            result(x)(y) = 255
          }
        }
      }

    }

    GrayscaleImage(width,height,result,input.originalImage)
  }
}
