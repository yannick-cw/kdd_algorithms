package data_structures

import org.scalatest.WordSpecLike

/**
  * Created by yannick on 12.04.16.
  */
class PointSpec extends WordSpecLike {
  "A Point" must {
    "return right results for addition and substraction" in {
      val x = Point(4,2)
      val y = Point(1,1)
      assert(x - y ==  Point(3,1))
      assert(x + y ==  Point(5,3))
    }

    "return right results for abs" in {
      assert(Point(-5,0).abs() == Point(5,0))
    }

    "return right result for multiplication" in {
      val x = Point(4,2)
      val y = Point(1,1)
      assert(x * y ==  6)
    }

    "return right result for manhatten distance" in {
      val x = Point(4,2)
      val y = Point(1,1)
      assert(x.distManhatten(y) == 4)
      assert(x.distManhatten(y) == y.distManhatten(x))
      assert(Point(0,0).distManhatten(Point(0,0)) == 0)
    }
    "return right result for euklidian distance" in {
      val x = Point(0,0)
      val y = Point(2,0)
      assert(x.distEuklid(y) == 2)

      val x2 = Point(1,1)
      val y2 = Point(2,2)
      assert(x2.distEuklid(y2) == 1.4142135623730951)
    }
  }

}
