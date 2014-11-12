package io.abacus.tallyho.rollingcounter

import org.cliffc.high_scale_lib.{Counter, NonBlockingHashMap}

import scala.collection.mutable.HashMap

// Not a streaming algorithm, as the storage is a function of the number of unique
// elements in the stream. Useful for when you know this is constant.
// Memory overhead: Number of unique items * number of buckets * size of counter

class RollingCounter[T](buckets:Int) {
  private val data = new NonBlockingHashMap[T,Array[Counter]]()
  private val currentBucket = new Counter

  def increment(thing:T) = {
    val index = currentBucket.longValue % buckets
    incrementWithBucket(thing,index.toInt)
  }

  def incrementWithBucket(thing:T, bucket:Int) = {
    val value = getBuckets(thing)
    value(bucket).increment

  }

  def count(thing:T):Long = {
    val array = data.get(thing)
    if(array == null) 0L
    else {
      var i = 0
      var sum = 0L
      while( i < buckets) {
        sum += array(i).estimate_get
        i = i+1
      }
      sum
    }
  }

  def counts:Map[T,Long] = {
    val keys = data.keySet
    val output = new HashMap[T,Long]()
    val it = keys.iterator
    while(it.hasNext) {
      val thing = it.next
      output.put(thing,count(thing))
    }

    output.toMap

  }

  def advanceBucket() {
    resetAllCountsForBucket(((currentBucket.get+1L) % buckets).toInt)
    currentBucket.increment


  }

  def resetAllCountsForBucket(bucket:Int) {
    val keys = data.keySet
    val it = keys.iterator
    while(it.hasNext) {
      val thing = it.next
      resetCountForBucket(thing,bucket)
    }

  }

  def resetCountForBucket(thing:T,bucket:Int) = {
    val value = getBuckets(thing)
    value(bucket) = new Counter
  }

  private def getBuckets(thing:T) = {
    val array = data.get(thing)
    if(array == null) initialCountsMaybe(thing) else array
  }

  private def initialCountsMaybe(thing:T):Array[Counter] ={
    val array = Array.fill[Counter](buckets)(new Counter)
    val previous = data.putIfAbsent(thing,array)
    if(previous == null)
    // This created the array, so return reference to array
      array
    else
      previous

  }

}
