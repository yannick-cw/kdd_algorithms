package clustering

import data_structures._
import scala.math.ceil

/**
  * Based on the variance minimizing algorithm of Lloyd
  * Result can be less than k sets, depending on the initial clustering
  */
object VarianceMinimizingLlyod extends ExtendedSetPreDef {

  def cluster(set: Set[Point], k: Int): Map[Point, Set[Point]] = {
    assert(k <= set.size)

    def go(currentCentroids: Set[Point]): Map[Point, Set[Point]] = {
      val pointWithCentroidTuple = set.map { point =>
        (point.getNearestCentroid(currentCentroids), point)
      }

      val groupedByMinCent = pointWithCentroidTuple
        .groupBy { case (centroid, point) => centroid }
        .map{ case (c, centPoint) => (c, centPoint.map{ case(centroid, point) => point})}

      val newCentroids: Set[Point] = groupedByMinCent.values.map(_.getCentroid()).toSet

      if (newCentroids == currentCentroids) groupedByMinCent
      else go(newCentroids)
    }


    val initialClusters = set.grouped(ceil(set.size.toDouble / k.toDouble).toInt)
    go(initialClusters.map(_ getCentroid()).toSet)
  }
}
