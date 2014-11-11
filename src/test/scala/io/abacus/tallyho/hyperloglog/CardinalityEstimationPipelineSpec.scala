package io.abacus.tallyho.hyperloglog

import org.scalatest.WordSpec
import org.scalatest.ShouldMatchers
import shapeless._

class CardinalityEstimationPipelineSpec extends WordSpec with ShouldMatchers {
  "The cardinality estimator" should {
    "count unique things with both StreamLib and Algebird" in {
      val data = List("apple", "orange", "apple", "banana", "persimmon")

      val streamPipe = new CardinalityEstimationPipeline(new StreamLibHLL())
      val algebirdPipe = new CardinalityEstimationPipeline(new AlgebirdHLL())

      val combinedPipe = streamPipe alongWith algebirdPipe
      data.foreach(combinedPipe.process)
      combinedPipe.results match {
        case streamCount :: algeCount :: HNil =>
          streamCount should be (algeCount)
          streamCount should be (4)
      }
    }
  }

}
