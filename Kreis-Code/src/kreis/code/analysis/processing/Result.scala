package kreis.code.analysis.processing

import java.awt.image.BufferedImage

/**
  * Ein Ergebnis einer Station
  */
trait Result {
  /**
    * Das Ergebnis als BufferedImage
    */
  val debugImage:BufferedImage
}
