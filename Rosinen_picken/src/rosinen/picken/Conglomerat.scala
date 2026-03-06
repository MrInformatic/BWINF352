package rosinen.picken


/**
  * Repräsentiert ein Konglomerat
  * @param companys Die Unternehmen des Konglomerates
  */
abstract class Conglomerat(var companys:Array[Company]) {
  /**
    * @return Gibt alle Unternehmen mit positivem absolutem Wert und die Unternehmen, die von diesen gekauft werden
    *         müssen, zurück.
    */
  def pickRaisins() : Array[Company];
}
