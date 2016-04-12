package data_structures

import org.scalatest.WordSpecLike

/**
  * Created by yannick on 12.04.16.
  */
class ExtendedSetSpec extends WordSpecLike with ExtendedSetPreDef {

  "ExtendedSetSpec" should {

    "return the right centroid" in {
      val set = Set(Point(0,0), Point(2,0), Point(2,2), Point(0,2))
      assert(set.getCentroid() == Point(1,1))

      assert(Set(Point(1,1)).getCentroid() == Point(1,1))

      assert(Set(Point(1,1), Point(-1,-1)).getCentroid() == Point(0,0))
    }

  }
}
