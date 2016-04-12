package clustering

import data_structures.Point
import org.scalatest.WordSpecLike

/**
  * Created by yannick on 12.04.16.
  */
class VarianceMinimizingSpec extends WordSpecLike {

  "VarianceMinimizingSpec" must {

    "cluster multiple points right" in {
      val point1 = Point(1, 1)
      val point2 = Point(0, 0)
      assert(VarianceMinimizing.cluster(Set(point1, point2), 1) == Set(Set(point1, point2)))

      val (a,b,c,d,e,f) =
        (Point(0,0),
          Point(1,1),
          Point(0,1),
          Point(4,5),
          Point(5,5),
          Point(6,6))

      val set = Set(d,a,c,f,e,b)
      assert(VarianceMinimizing.cluster(set, 2) == Set(Set(a,b,c), Set(d,e,f)))

      assert(VarianceMinimizing.cluster(Set(Point(1,1)), 1) == Set(Set(Point(1,1))))
    }

  }
}
