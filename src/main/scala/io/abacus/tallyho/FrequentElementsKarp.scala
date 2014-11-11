package io.abacus.tallyho
import org.cliffc.high_scale_lib.NonBlockingHashMap
import org.cliffc.high_scale_lib.NonBlockingHashSet
import org.cliffc.high_scale_lib.Counter

//Implementation of A Simple Algorithm for Finding Frequent Elements in Streams and Bags
// http://www.cs.yale.edu/homes/el327/datamining2011aFiles/ASimpleAlgorithmForFindingFrequentElementsInStreamsAndBags.pdf

class FrequentElementsKarp[T](theta:Double) {
  val K = new NonBlockingHashSet[T]()
  val count = new NonBlockingHashMap[T,Counter]()
  def process(x:T) = {
    if (K.contains(x)) {
      val c = count.get(x)
      c.increment()
    }
    else {
      K.add(x)
      val nc = new Counter()
      nc.increment()
      val old= count.putIfAbsent(x, nc)
      if(old != null) old.increment()
    }
    cleanup()

  }


  def results() = {K.toArray}

  def cleanup() = {
    if (K.size() > 1.0/theta) {
      val it = K.iterator()
      while(it.hasNext) {
        val v = it.next()
        count.get(v).decrement()
        val newCount = count.get(v).get()
        if (newCount == 0) K.remove(v)
      }
    }
  }
}
