package frequent_item_set

import scala.annotation.tailrec

/**
  * Created by yannick on 26.04.16.
  */
object APriori {

  def getFrequentItemSet[A](itemSets: Seq[Seq[A]], support: Int)(implicit ev: A => Ordered[A]): Seq[(Seq[A], Int)] = {
    val itemFrequency = itemSets.flatten.distinct.sorted.map(Seq(_))

    @tailrec
    def run(itemSet: Seq[Seq[A]], agg: Seq[(Seq[A], Int)]): Seq[(Seq[A], Int)] = itemSet match {
      case Nil => agg
      case lastElement if lastElement.size == 1 => agg
      case _ =>
        val candidates = generateCandidates(itemSet)
        val candidatesFrequency = getCandidatesFrequency(itemSets, candidates)
        val candidatesWithSupport = candidatesFrequency.filter(_._2 > support)
        run(candidatesWithSupport.map(_._1), agg ++ candidatesWithSupport)
    }
    run(itemFrequency,  Seq.empty[(Seq[A], Int)])
  }

  private def getCandidatesFrequency[A](itemSets: Seq[Seq[A]], candidates: Seq[Seq[A]]): Seq[(Seq[A], Int)] = {
    val candidatesFrequency = for {
      candidate <- candidates
      iSet <- itemSets
    } yield if (candidate.forall(iSet.contains(_))) Some(candidate) else None

    candidatesFrequency.flatten
      .groupBy(identity)
      .mapValues(_.size)
      .toSeq
  }

  private def generateCandidates[A](itemSets: Seq[Seq[A]]): Seq[Seq[A]] = {
    val itemSetLength = itemSets.head.length
    itemSets.flatMap { itemSet =>
      val pos = itemSets.indexOf(itemSet) + 1
      val prefixLength = itemSetLength - 1
      (pos until itemSets.size).flatMap { i =>
        itemSets(i) match {
          case set if set.take(prefixLength) == itemSet.take(prefixLength) => Some(itemSet :+ itemSets(i).last)
          case _ => None
        }
      }
    }
  }
}
