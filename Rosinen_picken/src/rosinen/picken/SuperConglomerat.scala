package rosinen.picken

import scala.collection.mutable

/**
  * Das Konglomerat
  * @param companysc Die Unternehmen des Konglomerates
  */
class SuperConglomerat(private var companysc:Array[Company]) extends Conglomerat(companysc){
  /**
    * Die Sub-Konglomerate des Konglomerates
    */
  val subConglomerates:Array[SubConglomerat] = getSubConglomerates();

  /**
    * Unterteilt das Konglomerat in Sub-Konglomerate
    * @return Die Sub-Konglomerate
    */
  private def getSubConglomerates() : Array[SubConglomerat] = {
    val analyzed:mutable.BitSet = new mutable.BitSet(companys.length);

    companys
      .toStream
      .filter(company => analyzed.add(company.id))
      .map(company => Stream.iterate(Stream(company))(_
          .flatMap(child =>  child.childs ++ child.parents)
          .filter(child => analyzed.add(child.id)))
        .takeWhile(childs => childs.nonEmpty)
        .flatMap(childs => childs))
      .tap(_.zipWithIndex.foreach(company => company._1.subConglomeratId = company._2))
      .map(_.toArray)
      .map(company => new SubConglomerat(company))
      .toArray
  }

  /**
    * @return Gibt alle Unternehmen mit positivem absolutem Wert und die Unternehmen, die von diesen gekauft werden
    *         müssen, zurück.
    */
  override def pickRaisins(): Array[Company] = subConglomerates.flatMap(_.pickRaisins())

}
