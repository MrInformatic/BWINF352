package kreis.code

import java.awt.geom.AffineTransform
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import kreis.code.analysis.encoder.BinaryToAsciiConverterLoader
import kreis.code.analysis.processing.Pipeline
import kreis.code.analysis.processing.analysableimage._
import kreis.code.analysis.processing.centralpoint.CentralPointImageProcessor
import kreis.code.analysis.processing.grayscaleimage.GrayscaleImageProcessor
import kreis.code.analysis.processing.imagefilter.{BoxBlurProcessor, LocalNormalizeProcessor, NormalizeProcessor}
import kreis.code.analysis.processing.kreiscode.{KreisCodeImage, KreisCodeImageProcessor}
import kreis.code.analysis.processing.originalimage.{OriginalImage, OriginalImageProcessor}
import kreis.code.analysis.processing.pixelgroupbounds.{PixelGroupBoundsImageProcessor, StackedPixelGroupProcessor}
import kreis.code.gui.DisplayFrame

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Main {
  def main(args : Array[String]) : Unit = {
    val binaryToAsciiConverter = BinaryToAsciiConverterLoader.load(new File(args(0)))
    val pipeline = new OriginalImageProcessor -->
      new GrayscaleImageProcessor -->
      new BoxBlurProcessor(4) -->
      new LocalNormalizeProcessor(256,64) -->
      new AnalysableImageProcessor(128) -->
      new PixelGroupBoundsImageProcessor(64) -->
      new StackedPixelGroupProcessor -->
      new CentralPointImageProcessor -->
      new KreisCodeImageProcessor(binaryToAsciiConverter,0.2,16)

    /*args
      .toIterator
      .drop(1)
      .map(i => (ImageIO.read(new File(i)),i))
      .map(i => (pipeline.produceAllResults(i._1),i._2))
      .foreach(i => i._1
        .zipWithIndex
        .foreach(n => ImageIO.write(n._1.debugImage,"jpg",new File("result"+(n._2+1)+"/"+i._2))))*/
    val display = new DisplayFrame(pipeline)
  }
}



