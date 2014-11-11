package io.abacus.tallyho.countminsketch

import org.scalatest.WordSpec
import org.scalatest.ShouldMatchers
import shapeless._

class CountMinSketchPipelineSpec extends WordSpec with ShouldMatchers {
  "The CMS Pipeline" should {
    "return frequency of elements" in {
      val data = List("apple", "orange", "apple", "banana", "persimmon")
      val cmsPipe = new CountMinSketchPipeline(new StreamCMS(4,10,1))
      data.foreach(cmsPipe.process)

      val results = cmsPipe.result
      results("apple") should be (2)
      results("orange") should be (1)
    }
  }

}
