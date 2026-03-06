package kreis.code.analysis.encoder

/**
  * Ein Baum, der eine Zahl in irgendetwas umwandelt
  * @param states Die Anzahl der Möglichkeiten pro Stelle
  * @param height Die Höhe des Baumes
  * @param default Die Standardausgabe, wenn kein Ergebnis gefunden werden konnte
  * @tparam T Das Irgendetwas
  */
class DecoderTree[T](val states:Int, val height:Int, val default:T) {
  /**
    * Die Wurzel des Baumes
    */
  private var top:Element = new Node;

  /**
    * Fügt einen Eintrag hinzu
    * @param depthStateMapper Eine Funktion, die eine Stelle der Zahl zurückgibt
    * @param value Das korrespondierende Ergebnis
    */
  def insertValue(depthStateMapper: Int => Int,value:T): Unit ={
    val node = Stream.iterate((top,0),height)(element => {
      val node = element._1.asInstanceOf[Node];
      val state = depthStateMapper(element._2);
      if(node.elements(state)==null){
        node.elements(state) = new Node
      }
      (node.elements(state),element._2+1)
    }).map(_._1).last.asInstanceOf[Node];
    val state = depthStateMapper(height-1);
    if(node.elements(state)==null){
      node.elements(state) = new End(value)
    }
  }

  /**
    * Wandelt eine Zahl in irgendetwas um
    * @param depthStateMapper Eine Funktion, die eine Stelle der Zahl zurückgibt
    * @return Das Ergebnis
    */
  def getValue(depthStateMapper: Int => Int): T ={
    try {
      Stream.iterate((top, 0), height)(element =>
        (element._1.asInstanceOf[Node].elements(depthStateMapper(element._2)), element._2 + 1))
        .map(_._1)
        .last
        .asInstanceOf[Node]
        .elements(depthStateMapper(height - 1))
        .asInstanceOf[End]
        .value
    }catch{
      case e: Throwable => default
    }
  }

  /**
    * Ein Element des Baumes
    */
  class Element(){

  }

  /**
    * Ein Knoten des Baumes
    */
  class Node() extends Element{
    /**
      * Alle Kindknoten des Knotens
      */
    val elements = new Array[Element](states);
  }

  /**
    * Ein Knoten auf der letzten Ebene des Baumes
    * @param value Das Ergebnis des Weges
    */
  class End(val value:T)extends Element{

  }
}
