package kreis.code.analysis.processing.kreiscode

import java.awt.{Color, Font}
import java.awt.image.BufferedImage

import kreis.code.analysis.processing.Result
import kreis.code.analysis.processing.originalimage.OriginalImage

/**
  * Ein Bild, welches die Kreiscodes speichert
  * @param width Die Breite des Bildes
  * @param height Die Höhe des Bildes
  * @param kreisCodes Die Kreiscodes
  * @param originalImage Das Orginalbild
  */
class KreisCodeImage(val width:Int,val height:Int,val kreisCodes: Array[KreisCode],val originalImage: OriginalImage) extends Result{
  kreisCodes.foreach(kc => println(kc.position.toString()+kc.data))

  /**
    * Das Bild als BufferedImage mit den Kreiscodes mit ihren repräsentativen Daten
    */
  lazy val debugImage: BufferedImage = {
    val oldimage = originalImage.debugImage
    val image = new BufferedImage(width,height,BufferedImage.TYPE_INT_RGB)

    val graphics = image.getGraphics

    graphics.drawImage(oldimage,0,0,null)

    graphics.setFont(new Font(graphics.getFont.getName,Font.BOLD,200))
    graphics.getFontMetrics
    graphics.setColor(Color.RED)
    for(kc <- kreisCodes){
      graphics.drawString(kc.data+"",kc.position.x,kc.position.y)
    }
    image
  }
}
