package rosinen.picken

import java.util.concurrent.atomic.AtomicInteger

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


/**
  * Ein Unternehmen
  * @param id Der Identifier des Unternehmens im Conglomerat
  * @param value Der Wert des Unternehmens
  */
class Company(val id:Int,val value:Double) {
  /**
    * Die Unternehmen, die dieses Unternehmen kaufen müssen
    */
  var parents:Array[Company] = Array();
  /**
    * Die Unternehmen, die dieses Unternehmen kaufen muss
    */
  var childs:Array[Company] = Array();
  /**
    * Der Identifier des Unternehmens im Subconglomerat
    */
  var subConglomeratId:Int = -1;
  /**
    * Der Identifiere eines Splitter-Unternehmen im Subconglomerat
    */
  var splitterId:Int = -1;
  /**
    * Der Identifiere eines Combiner-Unternehmen im Subconglomerat
    */
  var combinerId:Int = -1;
  /**
    * Ob ein Unternehmen in einer starken Zusammenhangskomponente enthalten ist
    */
  var inCycle:Boolean = false;

  /**
    * Zur Berechnung der Summe der Werte der Unternehmen, die nur von diesem Splitter-Unternehmen gekauft werden, also
    * von keinem anderen Splitter-Unternehmen
    * @return Die Summe
    */
  def getSplitterValue(): Double = childs.toStream.filterNot(_.isSplitter()).map(_.getSplitterValue()).sum[Double]+value;

  /**
    * Gibt die Unternehmen mit positiven absoluten Wert zurück
    * @param splitterValueMapper Eine Funktion, die die Summen der Werte der Unternehmen, die nur von dem übergegebenen
    *                            Splitter-Unternehmen gekauft werden, also von keinen anderen Splitter-Unternehmen
    * @param currentSplitters Gibt die Splitter-Unternehmen, die vom vorherigen Unternehmen gekauft werden müssen, an
    * @param currentValue Gibt die Summe der Werte aller Unternehmen, die von dem vorherigen Unternehmen gekauft werden
    *                     müssen, an
    * @param combinerSplitters Der Speicher für alle Splitter-Unternehmen, die von den Combiner-Unternehmen gekauft
    *                          werden müssen
    * @param combinerChildsProcessed Der Speicher für die Anzahl der verarbeiteten Kind-Knoten der
    *                                Combiner-Unternehmen
    * @param combinerValues Der Speicher für die absoluten Werte der Combiner-Unternehmen
    * @param raisins Unternehmen mit positiven absoluten Werten
    * @tparam S Die Set-Implementierung, die genutzt werden soll
    */
  def pickRaisins[S <: mutable.Set[Int]](splitterValueMapper: Int => Double,
                                         currentSplitters: S,
                                         currentValue: Double,
                                         combinerSplitters: Array[S],
                                         combinerChildsProcessed: Array[Int],
                                         combinerValues: Array[Double],
                                         raisins: S): Unit = {
    if(isCombiner()){
      combinerValues(combinerId) += currentValue-(combinerSplitters(combinerId) ++ currentSplitters)
        .map(splitterValueMapper).sum
      combinerSplitters(combinerId) ++= currentSplitters
      combinerChildsProcessed(combinerId) += 1
      if(combinerChildsProcessed(combinerId)==childs.length){
        if(combinerValues(combinerId)>0 && !inCycle){
          raisins += subConglomeratId
        }
        if(isSplitter()){
          combinerSplitters(combinerId) += splitterId;
          parents.foreach(_.pickRaisins(splitterValueMapper,
            combinerSplitters(combinerId).clone.asInstanceOf[S],
            combinerValues(combinerId)+value,
            combinerSplitters,
            combinerChildsProcessed,
            combinerValues,
            raisins));
        }else{
          parents.foreach(_.pickRaisins(splitterValueMapper,
            combinerSplitters(combinerId),
            combinerValues(combinerId)+value,
            combinerSplitters,
            combinerChildsProcessed,
            combinerValues,
            raisins));
        }
      }
    }else{
      if(currentValue+value > 0 && !inCycle){
        raisins += subConglomeratId;
      }
      if(isSplitter()){
        currentSplitters += splitterId;
        parents.foreach(_.pickRaisins(splitterValueMapper,
          currentSplitters.clone.asInstanceOf[S],
          currentValue+value,
          combinerSplitters,
          combinerChildsProcessed,
          combinerValues,
          raisins));
      }else{
        parents.foreach(_.pickRaisins(splitterValueMapper,
          currentSplitters,
          currentValue+value,
          combinerSplitters,
          combinerChildsProcessed,
          combinerValues,
          raisins));
      }
    }
  }

  /**
    * Markiert alle Unternehmen, die gekauft werden müssen im related-Set
    * @param related Das related-Set
    * @tparam S Die Set-Implemntierung, die genutzt werden soll
    */
  def getRaisinsRelated[S <: mutable.Set[Int]](related: S): Unit = {
    if(!related.contains(subConglomeratId)){
      related+=subConglomeratId;
      childs.foreach(_.getRaisinsRelated(related));
    }
  }

  /**
    * @return Gibt an, ob das Unternehmen ein Splitter-Unternehmen ist
    */
  def isSplitter() : Boolean = parents.length > 1

  /**
    * @return Gibt an, ob das Unternehmen kein Unternehmen kaufen muss
    */
  def isLowest() : Boolean = childs.length==0

  /**
    * @return Gibt an, ob das Unternehmen von keinem Unternehmen gekauft wird
    */
  def isHighest() : Boolean = parents.length==0;

  /**
    * @return Gibt an, ob das Unternehmen ein Combiner-Unternehmen ist
    */
  def isCombiner(): Boolean = childs.length > 1

  /**
    * Sucht nach starken Zusammenhangskomponenten
    * @param index Tiefensucheindex der Unternehmen
    * @param lowlink Kleinster Tiefensucheindex aller Unternehmen, die erreichbar sind
    * @param onStack Gibt an, ob ein Unternehmen auf dem Stack ist
    * @param i Zählervariable
    * @param stack Die Unternehmen liegen in der Reihenfolge, in der sie besucht werden, auf dem Stack
    * @param circles Starke Zusamenhangskomponenten
    */
  def strongconnect(index:Array[Int],
                    lowlink:Array[Int],
                    onStack:mutable.BitSet,
                    i:AtomicInteger,
                    stack: mutable.Stack[Company],
                    circles:ListBuffer[Array[Company]]): Unit = {

    index.update(subConglomeratId,i.get());
    lowlink.update(subConglomeratId,i.get());
    i.incrementAndGet();
    stack.push(this);
    onStack += subConglomeratId;

    childs
      .foreach(child => {
        if(index(child.subConglomeratId) == -1){
          child.strongconnect(index,lowlink,onStack,i,stack,circles)
          lowlink(subConglomeratId) = Math.min(lowlink(subConglomeratId),lowlink(child.subConglomeratId));
        }else if(onStack(child.subConglomeratId)){
          lowlink(subConglomeratId) = Math.min(lowlink(subConglomeratId),index(child.subConglomeratId));
        }
      });

    if(lowlink(subConglomeratId)==index(subConglomeratId)){
      val resultCollector = new ListBuffer[Company]();
      var next:Company = null;
      do{
        next = stack.pop();
        resultCollector += next;
        onStack -= next.subConglomeratId;
      }while(next != this)
      circles += resultCollector.toArray;
    }
  }
}
