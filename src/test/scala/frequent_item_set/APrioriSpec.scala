package frequent_item_set

import org.scalatest.WordSpecLike

/**
  * Created by yannick on 26.04.16.
  */
class APrioriSpec extends WordSpecLike {

  "APrioriSpec" should {

    "calculate correct itemsets for simple item list" in {
      val shopping = Seq(Seq("milk", "butter", "beer")
        , Seq("milk", "butter")
        , Seq("milk", "butter", "beer")
        , Seq("milk", "butter", "beer", "fish")
        , Seq("milk", "fish")
        , Seq("milk")
        , Seq("butter", "beer")
        , Seq("fish", "butter", "beer"))

      assert(APriori.getFrequentItemSet(shopping, 1) == Seq((List("beer", "fish"), 2),
        (List("butter", "fish"), 2),
        (List("beer", "milk"), 3),
        (List("beer", "butter"), 5),
        (List("butter", "milk"), 4),
        (List("fish", "milk"), 2),
        (List("beer", "milk", "butter"), 3),
        (List("beer", "fish", "butter"), 2)))

      assert(APriori.getFrequentItemSet(shopping, 2) == Seq(
        (List("beer", "milk"), 3),
        (List("beer", "butter"), 5),
        (List("butter", "milk"), 4),
        (List("beer", "milk", "butter"), 3)))

      assert(APriori.getFrequentItemSet(shopping, 3) == Seq(
        (List("beer", "butter"), 5),
        (List("butter", "milk"), 4)))
    }
  }
}
