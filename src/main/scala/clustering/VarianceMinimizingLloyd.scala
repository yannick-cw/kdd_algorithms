package clustering

import data_structures._
import scala.math.ceil

/**
  * Based on the variance minimizing algorithm of Lloyd
  * Result can be less than k sets, depending on the initial clustering
  */
object VarianceMinimizingLloyd extends ExtendedSetPreDef {

  def cluster(set: Set[Point], k: Int): Set[Set[Point]] = {
    assert(k <= set.size)

    def go(currentCentroids: Set[Point]): Set[Set[Point]] = {
      val pointWithCentroidTuple = set.map { point =>
        (getNearestCentroid(currentCentroids, point), point)
      }

      val groupedByMinCent = pointWithCentroidTuple
        .groupBy { case (centroid, point) => centroid }
        .map{ case (_, centPoint) => centPoint.map{ case(centroid, point) => point}}
        .toSet

      val newCentroids: Set[Point] = groupedByMinCent.map(_.getCentroid())

      if (newCentroids == currentCentroids) groupedByMinCent
      else go(newCentroids)
    }


    val initialClusters = set.grouped(ceil(set.size.toDouble / k.toDouble).toInt)
    go(initialClusters.map(_ getCentroid()).toSet)
  }


  private def getNearestCentroid(centroids: Set[Point], point: Point): Point = {
    centroids.map(cent => (cent, cent.distLeastSquares(point)))
      .minBy{case (centroid, dist) => dist}._1
  }
}
