package kreis.code.analysis.processing.kreiscode

import kreis.code._
import kreis.code.analysis.encoder.BinaryToAsciiConverter
import kreis.code.analysis.processing.Processor
import kreis.code.analysis.processing.centralpoint.CentralPointImage
import kreis.code.utils.{Matrix2D, Vector2D, Vector2I}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Dekodiert alle erkannten Kreiscodes
  *
  * @param binaryToAsciiConverter Wandelt Binärcodes in Zeichen um
  * @param deviation Gibt die Abweichung der Radien der Kreise voneinander an, die sie maximal haben dürfen
  * @param microsteps Gibt an, wie oft jeder Kreiscode gelesen werden soll
  * @tparam T Typ für den BinaryToAsciiConverter
  */
class KreisCodeImageProcessor[T[X] <: Traversable[X]](val binaryToAsciiConverter: BinaryToAsciiConverter[T],val deviation:Double/*,val radiusSteps:Int,val radiusDeviation:Double*/,val microsteps:Int) extends Processor[CentralPointImage,KreisCodeImage] {
  private val scanAngle = Math.PI / 8;
  lazy val name: String = "Kreis Code Image"

  /**
    * Dekodiert alle erkannten Kreiscodes
    * @param input Die mitelpunkte der Kreiscodes
    * @return Die dekodierten Kreiscodes
    */
  override def produce(input: CentralPointImage): KreisCodeImage = {
    val width = input.width
    val height = input.height
    val analysableImage = input.analysableImage

    /**
      * Gibt einen Vektor der Länge 1 zurück, der zwischen ihm und der X-Achse folgenden Winkel bildet
      * @param angle Der Winkel
      * @return Der Vektor
      */
    def getDirection(angle:Double):Vector2D =
      Vector2D(Math.sin(angle), Math.cos(angle))

    /**
      * Gibt die Punkte zurück, die ein Strahl, der von folgendem Punkt in folgende Richtung fliegt, im Bild schneidet
      * @param orgin Der Punkt, von dem aus der Strahl fliegt
      * @param dir Die Richtung, in die der Strahl fliegt
      * @return Die Punkte, durch die der Strahl fliegt
      */
    def bresenham(orgin: Vector2I, dir: Vector2D): Iterator[Vector2I] =
      Iterator.iterate(orgin())(i => i+dir)
        .map(i => i())
        .takeWhile(isPositionInImage(_))

    /**
      * Gibt einen Vektor zurück, dessen X- oder Y-Komponente 1 ist, und die jeweils andere kleiner als 1 ist, und in die gleiche
      * Richtung zeigt, wie der ursprüngliche Vektor
      * @param dir Der alte Vektor
      * @return Der neue Vektor
      */
    def normalizeBresenhalm(dir: Vector2D):Vector2D =
      dir / Math.max(Math.abs(dir.x), Math.abs(dir.y))

    object getUnit{
      /**
        * Gibt die Größe der Einheit zurück, die der Abbildung zur Aufgabe entspricht
        * @param orgin Die Mitte des Kreiscodes
        * @param angle Die Richtung, in die geprüft werden soll
        * @return Die Größe der Einheit
        */
      def apply(orgin:Vector2I,angle:Double): Double =
        getUnit(orgin,getDirection(angle))

      /**
        * Gibt die Größe der Einheit zurück, die der Abbildung zur Aufgabe entspricht
        * @param orgin Die Mitte des Kreiscodes
        * @param dir Die Richtung, in die geprüft werden soll
        * @return Die Größe der Einheit
        */
      def apply(orgin:Vector2I,dir:Vector2D): Double = {
        val normDir = normalizeBresenhalm(dir)
        getUnit(bresenham(orgin,normDir),normDir.len)
      }

      /**
        * Gibt die Größe der Einheit zurück, die der Abbildung zur Aufgabe entspricht
        * @param orgin Die Mitte des Kreiscodes
        * @param angle Die Richtung, in die geprüft werden soll
        * @param matrix Die Matrix, mit der die Richtung verzerrt werden soll
        * @return Die Größe der Einheit
        */
      def apply(orgin:Vector2I,angle:Double,matrix: Matrix2D): Double =
        getUnit(orgin,getDirection(angle),matrix)

      /**
        * Gibt die Größe der Einheit zurück, die der Abbildung zur Aufgabe entspricht
        * @param orgin Die Mitte des Kreiscodes
        * @param dir Die Richtung, in die geprüft werden soll
        * @param matrix Die Matrix, mit der die Richtung verzerrt werden soll
        * @return Die Größe der Einheit
        */
      def apply(orgin:Vector2I,dir:Vector2D,matrix: Matrix2D): Double =
        getUnit(orgin,matrix*dir)

      /**
        * Gibt die Größe der Einheit zurück, die der Abbildung zur Aufgabe entspricht
        * @param ray Ein Strahl, der von der Mitte des Kreiscodes in eine Richtung fliegt
        * @param dirLen Die Länge des Richtungsvektors des Strahls
        * @return Die Größe der Einheit
        */
      def apply(ray: Iterator[Vector2I],dirLen:Double): Double ={
        var innerCircleIndex = -2
        var outerCircleInnerIndex = 1
        var outerCircleOuterIndex = 2

        val blackWhiteChanges = ray
          .map(analysableImage(_))
          .sliding(2)
          .zipWithIndex
          .filter(i => i._1.head!=i._1.last)
          .map(_._2)
          .toList
          .toArray

        if(blackWhiteChanges.length < 3) return 0.0

        do {
          innerCircleIndex += 2
          if (innerCircleIndex >= blackWhiteChanges.length) return 0.0

          while ((blackWhiteChanges(innerCircleIndex) / 1.5 - blackWhiteChanges(outerCircleInnerIndex) / 2.5) / (blackWhiteChanges(innerCircleIndex) / 1.5) < -deviation) {
            outerCircleInnerIndex += 2
            if (outerCircleInnerIndex >= blackWhiteChanges.length) return 0.0
          }

          while ((blackWhiteChanges(innerCircleIndex) / 1.5 - blackWhiteChanges(outerCircleOuterIndex) / 3.5) / (blackWhiteChanges(innerCircleIndex) / 1.5) < -deviation) {
            outerCircleOuterIndex += 2
            if (outerCircleOuterIndex >= blackWhiteChanges.length) return 0.0
          }

        } while ((blackWhiteChanges(innerCircleIndex) / 1.5 - blackWhiteChanges(outerCircleInnerIndex) / 2.5) / (blackWhiteChanges(innerCircleIndex) / 1.5) > deviation ||
          (blackWhiteChanges(innerCircleIndex) / 1.5 - blackWhiteChanges(outerCircleOuterIndex) / 3.5) / (blackWhiteChanges(innerCircleIndex) / 1.5) > deviation)

        (blackWhiteChanges(innerCircleIndex) / 1.5 +
          blackWhiteChanges(outerCircleInnerIndex) / 2.5 +
          blackWhiteChanges(outerCircleOuterIndex) / 3.5) / 3 * dirLen
      }
    }

    /**
      * Gibt zurück, ob sich eine Position noch im Bild befindet
      * @param position Die Position
      * @return Das Ergebnis
      */
    def isPositionInImage(position:Vector2I): Boolean =
      position.x>=0&&position.x<width&&position.y>=0&&position.y<height

    object checkDirection{
      /**
        * Gibt zurück, ob in folgender Richtung eine 1 im Kreiscode codiert ist
        * @param orgin Die Mitte des Kreiscodes
        * @param angle Die Richtung, in die geprüft werden soll
        * @return Das Ergebnis
        */
      def apply(orgin:Vector2I,angle:Double):Boolean =
        checkDirection(orgin,getDirection(angle))

      /**
        * Gibt zurück, ob in folgender Richtung eine 1 im Kreiscode codiert ist
        * @param orgin Die Mitte des Kreiscodes
        * @param dir Die Richtung, in die geprüft werden soll
        * @return Das Ergebnis
        */
      def apply(orgin:Vector2I,dir:Vector2D):Boolean =
        checkDirection(bresenham(orgin,normalizeBresenhalm(dir)))

      /**
        * Gibt zurück, ob in folgender Richtung eine 1 im Kreiscode codiert ist
        * @param orgin Die Mitte des Kreiscodes
        * @param angle Die Richtung, in die geprüft werden soll
        * @param matrix Die Matrix, mit der die Richtung verzerrt werden soll
        * @return Das Ergebnis
        */
      def apply(orgin:Vector2I,angle:Double,matrix: Matrix2D):Boolean =
        checkDirection(orgin,getDirection(angle),matrix)

      /**
        * Gibt zurück, ob in folgender Richtung eine 1 im Kreiscode codiert ist
        * @param orgin Die Mitte des Kreiscodes
        * @param dir Die Richtung, in die geprüft werden soll
        * @param matrix Die Matrix, mit der die Richtung verzerrt werden soll
        * @return Das Ergebnis
        */
      def apply(orgin:Vector2I,dir:Vector2D,matrix: Matrix2D):Boolean =
        checkDirection(orgin,matrix*dir)

      /**
        * Gibt zurück, ob in folgender Richtung eine 1 im Kreiscode codiert ist
        * @param ray Ein Strahl, der von der Mitte des Kreiscodes in eine Richtung fliegt
        * @return Das Ergebnis
        */
      def apply(ray: Iterator[Vector2I]):Boolean ={
        var innerCircleIndex = -2
        var outerCircleInnerIndex = 1
        var outerCircleOuterIndex = 2
        var dataCircleInnerIndex = 3
        var dataCircleOuterIndex = 4

        val blackWhiteChanges = ray
          .map(analysableImage(_))
          .sliding(2)
          .zipWithIndex
          .filter(i => i._1.head!=i._1.last)
          .map(_._2)
          .toList
          .toArray

        if(blackWhiteChanges.length<5) return false

        do {
          innerCircleIndex += 2
          if (innerCircleIndex >= blackWhiteChanges.length) return false

          while ((blackWhiteChanges(innerCircleIndex) / 1.5 - blackWhiteChanges(outerCircleInnerIndex) / 2.5) / (blackWhiteChanges(innerCircleIndex) / 1.5) < -deviation) {
            outerCircleInnerIndex += 2
            if (outerCircleInnerIndex >= blackWhiteChanges.length) return false
          }

          while ((blackWhiteChanges(innerCircleIndex) / 1.5 - blackWhiteChanges(outerCircleOuterIndex) / 3.5) / (blackWhiteChanges(innerCircleIndex) / 1.5) < -deviation) {
            outerCircleOuterIndex += 2
            if (outerCircleOuterIndex >= blackWhiteChanges.length) return false
          }

          while ((blackWhiteChanges(innerCircleIndex) / 1.5 - blackWhiteChanges(dataCircleInnerIndex) / 4.5) / (blackWhiteChanges(innerCircleIndex) / 1.5) < -deviation) {
            dataCircleInnerIndex += 2
            if (dataCircleInnerIndex >= blackWhiteChanges.length) return false
          }

          while ((blackWhiteChanges(innerCircleIndex) / 1.5 - blackWhiteChanges(dataCircleOuterIndex) / 5.5) / (blackWhiteChanges(innerCircleIndex) / 1.5) < -deviation) {
            dataCircleOuterIndex += 2
            if (dataCircleOuterIndex >= blackWhiteChanges.length) return false
          }
        } while ((blackWhiteChanges(innerCircleIndex) / 1.5 - blackWhiteChanges(outerCircleInnerIndex) / 2.5) / (blackWhiteChanges(innerCircleIndex) / 1.5) > deviation ||
          (blackWhiteChanges(innerCircleIndex) / 1.5 - blackWhiteChanges(outerCircleOuterIndex) / 3.5) / (blackWhiteChanges(innerCircleIndex) / 1.5) > deviation ||
          (blackWhiteChanges(innerCircleIndex) / 1.5 - blackWhiteChanges(dataCircleInnerIndex) / 4.5) / (blackWhiteChanges(innerCircleIndex) / 1.5) > deviation ||
          (blackWhiteChanges(innerCircleIndex) / 1.5 - blackWhiteChanges(dataCircleOuterIndex) / 5.5) / (blackWhiteChanges(innerCircleIndex) / 1.5) > deviation)

        true
      }
    }

    new KreisCodeImage(width,height, input.centralPoints.map(p => {
      /*var maxUnit = (Vector2I(),0.0)
      var minUnit = (Vector2I(),Double.MaxValue)
      
      for(i <- 0 until radiusSteps){
        val dir = getDirection(2 * Math.PI * i / radiusSteps)
        val unit = getUnit(p,dir)
        if(unit>maxUnit._2) maxUnit = (dir,unit)
        if(unit<minUnit._2) minUnit = (dir,unit)
      }


      val matrix = if(minUnit._2*(1+radiusDeviation)<maxUnit._2*(1-radiusDeviation)) {
        Matrix2D(minUnit._1*minUnit._2,maxUnit._1*maxUnit._2)
      }else {
        Matrix2D()
      }*/

      val data =
        Iterator.range(0, microsteps).map(n =>
          Iterator.range(0, 16)
            .filter(i => checkDirection(p,i * scanAngle + (n * scanAngle / microsteps)/*,matrix*/))
            .foldLeft(new mutable.BitSet(16))((b, i) => {
              b += i
              b
            }))
          .map(binaryToAsciiConverter.convert)
          .foldLeft(Array.fill(256)(0))((a,i) =>{
            if(i!=binaryToAsciiConverter.default) {
              a(i) += 1
            }
            a
          })
          .zipWithIndex
          .maxBy(_._1)
          ._2.toChar

      new KreisCode(p, data)
    }), input.originalImage)
  }
}
