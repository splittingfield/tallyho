package io.abacus.tallyho.sampling

import org.scalatest.{ShouldMatchers, WordSpec}

import scala.util.Random

class ReservoirSamplingSpec extends WordSpec with ShouldMatchers {
  "The resevoir" should {
    "Add the first elements to smaller than the resevoir size" in {
      val a = new ReservoirSampling(5)
      a.process(1)
      a.process(2)
      a.results.take(2) should be (List(1,2))
    }
  }

  "Keep the last element after reaching the reservoir size and probability is lucky" in {
    val a = new ReservoirSampling(5, new Random(1))
    a.process(1)
    a.process(2)
    a.process(1)
    a.process(2)
    a.process(1)
    a.process(3)
    a.results.length should be (5)
    a.results should contain (3)

  }

}
