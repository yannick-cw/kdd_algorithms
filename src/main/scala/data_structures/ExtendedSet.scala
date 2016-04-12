package data_structures

/**
  * Created by yannick on 12.04.16.
  */
class ExtendedSet(val set: Set[Point]) {

  def getCentroid(): Point = {
    assert(set.nonEmpty)
    set.reduce(_ + _) / set.size
  }
}

trait ExtendedSetPreDef {
  implicit def setToExtendedSet(set: Set[Point]): ExtendedSet = new ExtendedSet(set)
}
