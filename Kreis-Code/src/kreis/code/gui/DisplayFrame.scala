package kreis.code.gui

import java.awt.event.{ActionEvent, ActionListener}
import java.awt.image.BufferedImage
import java.awt.{Dimension, DisplayMode, FlowLayout}
import java.io.File
import javax.imageio.ImageIO
import javax.swing._

import kreis.code._
import kreis.code.analysis.processing.{Pipeline, Result}
import kreis.code.analysis.processing.analysableimage.AnalyzableImageSet

/**
  * Ein Fenster zum Laden und Analysieren von Bildern
  * @param pipeline Die Pipeline
  * @tparam I Der Eingabetyp
  */
class DisplayFrame[I <: BufferedImage](val pipeline:Pipeline[I,_]) extends JFrame with ActionListener{
  private var results:Array[Result] = null;
  private var previousSelected = 0;

  setTitle("Display")
  setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  setSize(new Dimension(800,600))

  val jFileChooser = new JFileChooser(new File("."))

  val settingsMenuBar = new JMenuBar

  val fileMenu = new JMenu("File")

  val fileMenues = List("Open","Save","Quit")
    .map(new JMenuItem(_))
    .tap(fileMenu.add(_))
    .tap(_.addActionListener(this))
    .tap(i => i.setActionCommand(i.getName))
    .toArray

  val stepMenu = new JMenu("Step")

  val stepButtonGroup = new ButtonGroup

  val stepMenues = pipeline.segNames
    .map(new JRadioButtonMenuItem(_))
    .tap(stepMenu.add(_))
    .tap(stepButtonGroup.add(_))
    .tap(_.addActionListener(this))
    .zipWithIndex
    .tap(i => i._1.setActionCommand(i._2.toString))
    .map(_._1)
    .toArray

  stepMenues(0).setSelected(true)

  settingsMenuBar.add(fileMenu)
  settingsMenuBar.add(stepMenu)

  setJMenuBar(settingsMenuBar)

  val imageView = new ImageView
  val scrollPane = new JScrollPane(imageView)

  setContentPane(scrollPane)

  setVisible(true)

  /**
    * Zeigt das Bild und die Ergebnisse der einzelnen Analyseschritte an
    * @param image Das Eingabebild
    */
  def display(image: I): Unit ={
    results = pipeline.produceAllResults(image).toArray
    update
  }

  override def actionPerformed(actionEvent: ActionEvent): Unit = {
    if(actionEvent.getSource.isInstanceOf[JRadioButtonMenuItem]&&results!=null) {
      previousSelected = actionEvent.getActionCommand.toInt
      update
    }else{
      actionEvent.getActionCommand match {
        case "Open" => {
          if(jFileChooser.showOpenDialog(this)==JFileChooser.APPROVE_OPTION){
            display(ImageIO.read(jFileChooser.getSelectedFile).asInstanceOf[I])
          }
        }
        case "Save" => {
          if(jFileChooser.showSaveDialog(this)==JFileChooser.APPROVE_OPTION){
            ImageIO.write(imageView.image,"png",jFileChooser.getSelectedFile)
          }
        }
        case "Quit" => dispose()
      }
    }
  }

  /**
    * Aktualisiert das Bild
    */
  def update: Unit ={
    imageView.image = results.apply(previousSelected).debugImage
    imageView.repaint()
  }
}
