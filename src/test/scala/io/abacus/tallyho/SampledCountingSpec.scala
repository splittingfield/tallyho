//package io.abacus.tallyho
//
//import org.scalatest.{ShouldMatchers, WordSpec}
//import scala.util.Random
//
//class SampledCountingSpec extends WordSpec with ShouldMatchers {
//  "The Sampling counter" should {
//    "estimate the number of distinct elements when they are randomized" in {
//
//      val elements = List.fill(10)(Random.nextInt(99) + 1)
//      val counter = new SampledCounting(100)
//      elements.take(10).foreach(counter.process(_))
//      assert(false, s"Actual: ${elements.distinct.length}  Estimated: ${counter.estimate}")
//    }
//
//
//    "estimate the number of distinct elements when they are not randomized" in {
//      val elements = List(1, 2, 3, 4, 5)
//      val counter = new SampledCounting(100)
//      elements.take(5).foreach(counter.process(_))
//      assert(false, s"Actual: ${elements.distinct.length}  Estimated: ${counter.estimate}")
//    }
//
//  }
//  "The Sampling Hashing counter" should {
//    "estimate the number of distinct elements when they are randomized" in {
//
//      val elements = List.fill(10)(Random.nextInt(97) + 1)
//      val counter = new SampledCounting(97)
//      elements.take(10).foreach(counter.process(_))
//      assert(false, s"Actual: ${elements.distinct.length}  Estimated: ${counter.estimate}")
//    }
//
//
//    "estimate the number of distinct elements when they are not randomized" in {
//      val elements = List(1, 2, 3, 4, 5)
//      val counter = new SampledCounting(97)
//      elements.take(5).foreach(counter.process(_))
//      assert(false, s"Actual: ${elements.distinct.length}  Estimated: ${counter.estimate}")
//    }
//  }
//
//}
