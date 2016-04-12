package clustering

import data_structures.{ExtendedSetPreDef, Point}

/**
 * k-means clustering from the MacQueen algorithm
 */
object KMeans extends ExtendedSetPreDef {
  def cluster(set: Set[Point], k: Int): Map[Point, Set[Point]] = {
    assert(k <= set.size)

    def go(cluster: Map[Point, Set[Point]], points: Set[Point]): Map[Point, Set[Point]] = points match {
      case p if p.isEmpty => cluster
      case ps => go(update(ps.head, cluster), ps.tail)

    }

    def update(point: Point, cluster: Map[Point, Set[Point]]): Map[Point, Set[Point]] = {
      val nearestCentroid = point.getNearestCentroid(cluster.keySet)
      val newGroup = cluster.get(nearestCentroid).get + point
      (cluster - nearestCentroid) + (newGroup.getCentroid() -> newGroup)
    }

    val (initialCluster, points) = set.splitAt(k)
    val clusterMap: Map[Point, Set[Point]] = initialCluster.map(p => (p, Set(p))).toMap

    go(clusterMap, points)
  }
}
