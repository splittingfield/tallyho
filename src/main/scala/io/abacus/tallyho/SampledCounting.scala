package io.abacus.tallyho
import scala.collection.mutable.Set
import scala.util.Random

class SampledCounting(maxValue:Int) {
  val valuesSeen = Set.empty[Int]
  def process(elem:Int) = {
    valuesSeen += elem
  }

  def estimate = maxValue/valuesSeen.min - 1

}


class SampledCountingWithHashing(prime:Int) {
  val a = Random.nextInt(prime-1)
  val b = Random.nextInt(prime-1)
  val valuesSeen = Set.empty[Int]

  def process(elem:Int) = valuesSeen += (a*elem+b) % prime
  def estimate = prime/valuesSeen.min - 1


}




object RunSampledCounting extends App {
  val maxValue = 10

  for (i <- 1 to 50) {
    val elements = List.fill(500)(Random.nextInt(maxValue)+1)
    val noHash = new SampledCounting(maxValue)
    val withHash = new SampledCountingWithHashing(1033)
    elements.take(10).foreach{ s => noHash.process(s); withHash.process(s)}
    println(s" Actual: ${elements.distinct.length}   noHash ${noHash.estimate}, withHash ${withHash.estimate}")
  }
}