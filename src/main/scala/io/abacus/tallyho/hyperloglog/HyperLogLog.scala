package io.abacus.tallyho.hyperloglog



import com.clearspring.analytics.stream.cardinality.HyperLogLog

import io.abacus.tallyho.pipeline.SimplePipeline

import com.twitter.algebird.HyperLogLog._
import com.twitter.algebird.{HLL, HyperLogLogMonoid}


trait HyperLogLogInterface {
  def process(elem:String):Unit
  def estimate:Long
}


class AlgebirdHLL() extends HyperLogLogInterface  {
  val hll = new HyperLogLogMonoid(12)
  var sumHll = hll.zero

  def process(elem:String) = {
    val item = hll(elem.getBytes)
    sumHll = hll.plus(sumHll, item)
  }
  def estimate = {
    val approxSize = hll.sizeOf(sumHll)
    approxSize.estimate
  }


}

class StreamLibHLL() extends HyperLogLogInterface  {
  val hll = new HyperLogLog(12)

  def estimate =  hll.cardinality()

  def process(elem:String) = hll.offer(elem)


}



class CardinalityEstimationPipeline[T<:HyperLogLogInterface](hll:T) extends SimplePipeline[String,String,Long] {

  override def result = hll.estimate

  override def step(elem: String): String = {
    hll.process(elem)
    elem
  }
}

