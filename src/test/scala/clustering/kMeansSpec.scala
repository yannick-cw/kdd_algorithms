package clustering

import data_structures.Point
import org.scalatest.WordSpecLike

/**
  * Created by yannick on 12.04.16.
  */
class kMeansSpec extends WordSpecLike {

  "kMeansSpec" should {

    "cluster" in {
      val point1 = Point(1, 1)
      val point2 = Point(0, 0)
      val cluster = Point(0.5, 0.5)
      assert(KMeans.cluster(Set(point1, point2), 1) == Map(cluster -> Set(point1, point2)))

      val (a, b, c, d, e, f) =
        (Point(0, 0),
          Point(1, 1),
          Point(0, 1),
          Point(4, 5),
          Point(5, 5),
          Point(6, 6))
      val cluster1 = Point(5.0, 5.333333333333333)
      val cluster2 = Point(0.3333333333333333, 0.6666666666666666)

      val set = Set(d, a, c, f, e, b)
      assert(KMeans.cluster(set, 2) == Map(cluster1 -> Set(d, e, f), cluster2 -> Set(a, b, c)))

      assert(KMeans.cluster(Set(Point(1, 1)), 1) == Map(Point(1,1) -> Set(Point(1, 1))))
    }

  }
}
