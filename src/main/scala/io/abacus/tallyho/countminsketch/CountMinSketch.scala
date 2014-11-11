package io.abacus.tallyho.countminsketch

import com.clearspring.analytics.stream.frequency.CountMinSketch
import io.abacus.tallyho.pipeline.SimplePipeline

// Only implemented CMS from clearspring, as algebird only supports items of type long (I think)

trait CountMinSketchInterface {
  def process(elem:String):Unit
  def frequency(elem:String):Long
}

class StreamCMS(depth:Int, width:Int, seed:Int) extends CountMinSketchInterface {
  val cms = new CountMinSketch(depth,width,seed)

  override def process(elem:String):Unit = cms.add(elem,1)
  override def frequency(elem:String) = cms.estimateCount(elem)

}


class CountMinSketchPipeline[T <: CountMinSketchInterface](cms:T) extends SimplePipeline[String, String, String => Long] {
  override def step(elem:String) = {
    cms.process(elem)
    elem
  }

  override def result:String=>Long = (x:String) => cms.frequency(x)

}