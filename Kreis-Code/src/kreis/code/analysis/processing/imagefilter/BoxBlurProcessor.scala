package kreis.code.analysis.processing.imagefilter

import java.awt.image._

import kreis.code.analysis.processing.Processor
import kreis.code.analysis.processing.grayscaleimage.GrayscaleImage
import kreis.code.analysis.processing.originalimage.OriginalImage

import scala.collection.parallel.ParIterable
import scala.collection.parallel.immutable.ParRange
import scala.collection.parallel.mutable.ParArray

/**
  * Erzeugt ein unscharfes Bild aus einem Anderen mithilfe des Box-Blur-Algorithmus
  * @param radius Der Radius des Box-Blurs
  */
class BoxBlurProcessor(val radius:Int) extends Processor[GrayscaleImage,GrayscaleImage]{
  /**
    * Der Name der Station
    */
  lazy val name: String = "Box Blur Image"

  /**
    * Erzeugt ein unscharfes Bild aus einem Anderen mithilfe des Box-Blur-Algorithmus
    * @param input Das Bild
    * @return Das unscharfe Bild
    */
  override def produce(input: GrayscaleImage): GrayscaleImage = {
    val width = input.width
    val height = input.height

    def isPositionInImage(position:(Int,Int)): Boolean =
      position._1>=0&&position._1<width&&position._2>=0&&position._2<height

    val result1 = Array.ofDim[Int](width,height)
    val result2 = Array.ofDim[Int](width,height)

    for(x <- 0 until width){
      var size = 0
      var sum = 0
      for(y <- 0 until radius){
        if(isPositionInImage(x,y)){
          sum += input(x,y)
          size += 1
        }
      }
      for(y <- 0 until height){
        if(isPositionInImage(x,y+radius)){
          sum += input(x,y+radius)
          size += 1
        }
        if(isPositionInImage(x,y-radius-1)){
          sum -= input(x,y-radius-1)
          size -= 1
        }
        result1(x)(y) = sum/size
      }
    }

    for(y <- 0 until height){
      var size = 0
      var sum = 0
      for(x <- 0 until radius){
        if(isPositionInImage(x,y)){
          sum += result1(x)(y)
          size += 1
        }
      }
      for(x <- 0 until width){
        if(isPositionInImage(x+radius,y)){
          sum += result1(x+radius)(y)
          size += 1
        }
        if(isPositionInImage(x-radius-1,y)){
          sum -= result1(x-radius-1)(y)
          size -= 1
        }
        result2(x)(y) = sum/size
      }
    }

    GrayscaleImage(width,height,result2,input.originalImage)
  }
}
