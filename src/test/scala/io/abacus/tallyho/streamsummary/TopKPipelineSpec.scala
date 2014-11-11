package io.abacus.tallyho.streamsummary

import org.scalatest.WordSpec
import org.scalatest.ShouldMatchers
import shapeless._

class TopKPipelineSpec extends WordSpec with ShouldMatchers {
  "The TopK pipeline" should {
    "compute the top K with algebird and streamlib" in {
      val data = List("apple", "orange", "apple", "banana", "persimmon", "apple", "persimmon")

      val streamPipe = new TopKPipeline(2, new StreamLibStreamSummary(10))
      val algebirdPipe = new TopKPipeline(2, new AlgebirdStreamSummary(10))

      val combinedPipe = streamPipe alongWith algebirdPipe
      data.foreach(combinedPipe.process)

      combinedPipe.results match {
        case streamTopK::algebirdTopK::HNil =>
          streamTopK should be (algebirdTopK)
          streamTopK.head should be (("apple", 3))
      }
    }
  }

}
