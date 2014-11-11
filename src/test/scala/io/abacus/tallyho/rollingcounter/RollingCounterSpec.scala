package io.abacus.tallyho.rollingcounter

import org.scalatest.WordSpec
import org.scalatest.ShouldMatchers
import org.scalatest.ParallelTestExecution

class RollingCounterSpec extends WordSpec with ShouldMatchers with ParallelTestExecution {
  "A Windowed Counter" should {
    "return 0 when no objects are in the counter" in {
      val a = new RollingCounter[String](2)
      a.count("hello") should be (0)
    }

    "increment the count by 1" in {
      val a = new RollingCounter[String](2)
      a.increment("a")
      a.count("a") should be (1)
    }

    "return a map of counts for objects" in {
      val a = new RollingCounter[String](2)
      a.increment("a")
      a.increment("b")
      a.increment("a")
      a.counts should be (Map("a"->2, "b"->1))
    }

    "reset the count for a key and bucket" in {
      val a = new RollingCounter[String](2)
      a.increment("a")
      a.resetCountForBucket("a",0)
    }

    "advance the current bucket by 1 with 2 total buckets" in {
      val a = new RollingCounter[String](2)
      a.increment("a")
      a.advanceBucket
      a.increment("a")
      a.count("a") should be (2)
    }

    "advance the current bucket by 2 and wrap around" in {
      val a = new RollingCounter[String](2)
      a.increment("a")
      a.increment("b")
      a.advanceBucket
      a.increment("a")
      a.advanceBucket
      a.counts should be (Map("a"->1, "b"->0))
    }
  }

}
