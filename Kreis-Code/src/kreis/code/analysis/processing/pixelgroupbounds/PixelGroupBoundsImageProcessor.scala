package kreis.code.analysis.processing.pixelgroupbounds

import java.util.concurrent.atomic.AtomicInteger

import kreis.code._
import kreis.code.analysis.processing.{Pipeline, Processor}
import kreis.code.analysis.processing.analysableimage.{AnalysableImage, AnalyzableImageSet}
import kreis.code.utils.Vector2I

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Findet die Figuren im Zweifarben-Bild
  * @param minSize Die minimale Größe einer Figur
  */
class PixelGroupBoundsImageProcessor(val minSize:Int) extends Processor[AnalysableImage,PixelGroupBoundsImage]{
  /**
    * Der Name der Station
    */
  lazy val name: String = "Pixel Group Image"

  /**
    * Findet die Figuren im Zweifarben-Bild
    * @param input Das Zweifarben-Bild
    * @return Die Figuren
    */
  override def produce(input:AnalysableImage):PixelGroupBoundsImage = {
    val width = input.width
    val height = input.height

    val analyzed = AnalyzableImageSet.getBitSetImpl(input.width,input.height,null);

    /**
      * Gibt zurück, ob sich eine Position noch im Bild befindet
      * @param position Die Position
      * @return Das Ergebnis
      */
    def isPositionInImage(position:Vector2I): Boolean =
      position.x>=0&&position.x<width&&position.y>=0&&position.y<height

    /**
      * Gibt die maximale und minimale X- und Y-Position einer Figur zurück
      * @param start Die Startposition der Tiefensuche
      * @param size Ein Zähler, der zählt, wieviele Pixel die Figur umfasst
      * @return Maximale und minimale X- und Y-Position der Figur
      */
    def getPixelGroup(start:Vector2I, size:AtomicInteger):(Vector2I,Vector2I) ={
      if(isPositionInImage(start) && input(start) && !analyzed(start)){
        analyzed += start
        //Minimale und maximale X- und Y-Positionen der Nachbarn ermitteln
        val minAndMaxes = ListBuffer((1,0),(-1,0),(0,1),(0,-1))
          .map(dir => start+dir)
          .map(getPixelGroup(_,size))
        size.incrementAndGet()
        //Einbeziehung der aktuellen Position in die Betrachtung
        var minX = start.x
        var minY = start.y
        var maxX = start.x
        var maxY = start.y
        //Ermitteln der neuen minimalen und maximalen X- und Y-Position
        for((otherMin,otherMax) <- minAndMaxes){
          if(otherMin.x<minX) minX = otherMin.x
          if(otherMin.y<minY) minY = otherMin.y
          if(otherMax.x>maxX) maxX = otherMax.x
          if(otherMax.y>maxY) maxY = otherMax.y
        }

        ((minX,minY),(maxX,maxY))
      }else{
        //Etwas, was immer kleiner oder größer als was anderes ist, und deshalb immer aus der Betrachtung ausgeschlossen
        //wird
        ((Int.MaxValue,Int.MaxValue),(Int.MinValue,Int.MinValue))
      }
    }

    val pixelGroups = new ListBuffer[PixelGroupBounds]

    for(x <- 0 until width;y <- 0 until height){
      if(input(x,y) && !analyzed(x, y)){
        val size = new AtomicInteger()
        val data = getPixelGroup((x,y),size)

        if(size.get>minSize) {
          val minX = data._1.x
          val minY = data._1.y
          val maxX = data._2.x
          val maxY = data._2.y

          pixelGroups += new PixelGroupBounds(minX, minY, maxX - minX + 1, maxY - minY + 1)
        }
      }
    }

    new PixelGroupBoundsImage(width,height,pixelGroups.toArray,input,input.originalImage)
  }
}
