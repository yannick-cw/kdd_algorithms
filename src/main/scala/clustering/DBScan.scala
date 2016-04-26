package clustering

import data_structures.Point

/**
  * Created by yannick on 24.04.16.
  */
object DBScan {
  def cluster(points: Set[Point], e: Double, minPts: Int): Seq[Set[Point]] = {
    val isClassified: Set[(Point, Boolean)] = points.map((_, false))




    ???
  }

}
