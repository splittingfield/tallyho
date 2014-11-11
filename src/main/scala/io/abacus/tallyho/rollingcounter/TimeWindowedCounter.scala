package io.abacus.tallyho.rollingcounter

import java.util.concurrent.{TimeUnit, Executors}

// window is size of the window in milliseconds and
// granularity is the size of each interval in milliseconds
// window/granularity should be even, if not, we round up
class TimeWindowedCounter[T](window:Long,granularity:Long) {
  private val buckets = window/granularity
  private val counter = new RollingCounter[T](buckets.toInt)
  private val scheduledThreadPool = Executors.newScheduledThreadPool(1)

  scheduledThreadPool.scheduleWithFixedDelay(new HeartBeat(counter),granularity,granularity,TimeUnit.MILLISECONDS)

  def increment(thing:T) = counter.increment(thing)
  def counts = counter.counts

  def stop = {
    scheduledThreadPool.shutdown
    scheduledThreadPool.awaitTermination(granularity,TimeUnit.MILLISECONDS)
    counts
  }

  class HeartBeat(counter:RollingCounter[T]) extends Runnable {
    def run() = {
      counter.advanceBucket
    }
  }


}