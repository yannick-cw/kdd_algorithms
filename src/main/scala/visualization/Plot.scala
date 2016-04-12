package visualization

import data_structures.Point
import org.sameersingh.scalaplot.Implicits._
import org.sameersingh.scalaplot.Style.Color
import org.sameersingh.scalaplot.XYPlotStyle


/**
  * Created by yannick on 12.04.16.
  */
object Plot {
  def plotClusterData(inData: Map[Point, Set[Point]], chartPath: String): Unit = {
    val centroids = inData.keySet.map(p => (p.x, p.y)).toSeq
    val sets = inData.values.map(_.toSeq.map(p => (p.x, p.y))).toSeq
    val xyDatas = sets.indices.map{ i =>
      XY(sets(i), style = XYPlotStyle.Points, color = Color.get((i + 2) % Color.values.size))
    }
    val xyDatasCentroids = centroids.indices.map{ i =>
      XY(Seq(centroids(i)), style = XYPlotStyle.Points, color = Color.get((i + 2) % Color.values.size), ps = 4.0)
    }
    output(PNG(chartPath, "test"), xyChart(xyDatas ++ xyDatasCentroids))
  }
}
