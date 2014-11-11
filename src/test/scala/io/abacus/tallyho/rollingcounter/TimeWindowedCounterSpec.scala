package io.abacus.tallyho.rollingcounter

import org.scalatest.WordSpec
import org.scalatest.ParallelTestExecution
import org.scalatest.ShouldMatchers

class TimeWindowedCounterSpec extends WordSpec with ShouldMatchers with ParallelTestExecution {
  "The TimeWindowsCounter" should {
    "not be too crazy" in {
      val a = new TimeWindowedCounter[String](100,10)
      a.increment("a")
      a.increment("b")
      a.increment("a")
      a.counts should be (Map("a"->2,"b"->1))
      Thread.sleep(30)
      a.increment("b")
      a.counts should be (Map("a"->2, "b"->2))
      Thread.sleep(80)
      a.counts should be (Map("b"->1, "a"->0))


    }

  }

}
