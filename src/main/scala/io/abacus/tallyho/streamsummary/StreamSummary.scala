package io.abacus.tallyho.streamsummary

import java.util.concurrent.atomic.{AtomicReference, AtomicBoolean}
import com.clearspring.analytics.stream.{StreamSummary, ConcurrentStreamSummary}
import com.twitter.algebird.{SpaceSaver, SpaceSaverSemigroup}
import io.abacus.tallyho.pipeline.SimplePipeline

import scala.collection.JavaConverters._


trait TopKInterface {
  def process(elem:String):Unit

  def top(k:Int):Seq[(String,Long)]
}

class StreamLibStreamSummary() extends TopKInterface {
  val topK = new StreamSummary[String](10)
  def process(elem:String) = {
    topK.offer(elem)
  }

  def top(k:Int) = {
    val a = topK.topK(k)
    a.asScala.map( a => (a.getItem, a.getCount)).toVector
  }

}

class AlgebirdStreamSummary() extends TopKInterface {
  val ssm = new SpaceSaverSemigroup[String]
  val summary = new AtomicReference[SpaceSaver[String]]()
  def process(elem:String) = {
    if(!(summary.get eq null) || !summary.compareAndSet(null, SpaceSaver(10, elem)))
      summary.set(ssm.plus(summary.get, SpaceSaver(10,elem)))
  }

  def top(k:Int) = {
    Option(summary.get).fold[Seq[(String,Long)]](Seq.empty)(_.topK(k).map(v => (v._1, v._2.estimate)))
  }

}


class TopKPipeline[T <: TopKInterface](k:Int, ss:T) extends SimplePipeline[String,String,Seq[(String,Long)]] {

  def step(elem:String) = {
    ss.process(elem)
    elem
  }

  override def result = ss.top(k)
}
