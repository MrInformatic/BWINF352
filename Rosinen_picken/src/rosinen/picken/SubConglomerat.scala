package rosinen.picken

import java.util.concurrent.atomic.AtomicInteger

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.collection.parallel.mutable.ParArray

/**
  * Ein Sub-Konglomerat
  * @param companysc Die Unternehmen des Sub-Konglomerates
  */
class SubConglomerat(private var companysc:Array[Company]) extends Conglomerat(companysc){
  /**
    * Verbindet alle starken Zusammenhangskomponenten des Sub-Konglomerats neu.
    */
  reconnectCircles(tarjan());

  /**
    * Verbindet mehrere starke Zusammenhangskomponeten neu
    * @param cycles Die starken Zusammenhangskomponenten
    */
  private def reconnectCircles(cycles: Array[Array[Company]]): Unit = cycles.foreach(reconnectCircle(_))

  /**
    * Verbindet eine starke Zusammenhangskomponente neu
    * @param circle Die starke Zusammenhangskomponete
    */
  private def reconnectCircle(circle: Array[Company]): Unit = {
    circle.tail
      .foreach(_.inCycle = true)

    val circleSet: mutable.BitSet = new mutable.BitSet(companys.length);

    circle
      .map(_.subConglomeratId)
      .foreach(circleSet += _);

    val parentSet: mutable.BitSet = new mutable.BitSet(companys.length);
    val childSet: mutable.BitSet = new mutable.BitSet(companys.length);

    circle.head.parents = circle
      .toStream
      .flatMap(_.parents)
      .filterNot(company => circleSet.contains(company.subConglomeratId))
      .filter(company => parentSet.add(company.subConglomeratId))
      .toArray

    circle.last.childs = circle
      .toStream
      .flatMap(_.childs)
      .filterNot(company => circleSet.contains(company.subConglomeratId))
      .filter(company => childSet.add(company.subConglomeratId))
      .toArray

    circle
      .toStream
      .sliding(2)
      .foreach(element => {
        element.head.childs = Array(element.last);
        element.last.parents = Array(element.head);
      });
  }

  /**
    * Sucht alle starken Zusamenhangskomponeten
    * @return Alle starken Zusammenhangskomponenten
    */
  private def tarjan() : Array[Array[Company]] = {
    val index: Array[Int] = Array.fill(companys.length)(-1)
    val lowlink: Array[Int] = Array.fill(companys.length)(-1)
    val onStack: mutable.BitSet = new mutable.BitSet(companys.length)
    val i:AtomicInteger = new AtomicInteger()
    val stack: mutable.Stack[Company] = new mutable.Stack[Company]()
    val circles: ListBuffer[Array[Company]] = new ListBuffer[Array[Company]];
    companys
      .toStream
      .filter(company => index(company.subConglomeratId) == -1)
      .foreach(_.strongconnect(
        index,
        lowlink,
        onStack,
        i,
        stack,
        circles
      ));
    return circles.filterNot(_.length < 2).toArray;
  }

  /**
    * @return Gibt alle Unternehmen mit positivem absolutem Wert und die Unternehmen, die von diesen gekauft werden
    *         müssen, zurück.
    */
  override def pickRaisins(): Array[Company] = {
    if(companys.length==1){
      if(companys(0).value>0) {
        return companys;
      }
    }else{
      val splitters: Array[Company] = companys.filter(_.isSplitter());
      splitters.toStream.zipWithIndex.foreach(company => company._1.splitterId = company._2);

      val combiners: Array[Company] = companys.filter(_.isCombiner());
      combiners.toStream.zipWithIndex.foreach(company => company._1.combinerId = company._2);

      val combinerSplitters = Array.fill(combiners.length)(new mutable.BitSet(combiners.length));
      val combinerChildsProcessed = Array.fill(combiners.length)(0);
      val combinerValues = Array.fill(combiners.length)(0d);

      val raisins:mutable.BitSet = new mutable.BitSet(companys.length)

      val splitterValues:Array[Double] = splitters
        .toStream
        .map(_.getSplitterValue())
        .toArray;

      companys
        .toStream
        .filter(_.isLowest())
        .foreach(_.pickRaisins(
          splitterValues(_),
          new mutable.BitSet(companys.length),
          0,
          combinerSplitters,
          combinerChildsProcessed,
          combinerValues,
          raisins
      ));

      val raisinRelated:mutable.BitSet = new mutable.BitSet(companys.length);

      raisins
        .toStream
        .map(companys(_))
        .foreach(_.getRaisinsRelated(raisinRelated));

      return raisinRelated.toArray.map(companys(_));
    }

    return Array();
  }
}
