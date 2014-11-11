package io.abacus.tallyho.sampling

import org.cliffc.high_scale_lib.Counter
import scala.util.Random

class ReservoirSampling(reservoirSize:Int, random:Random = new Random()) {
  val count = new Counter()
  // Assume elements are positive for pedagogy
  val resevoir = Array.fill(reservoirSize)(-1)


  def process(elem:Int) = {
    val index = count.get()
    if(index < reservoirSize)
      resevoir(index.toInt) = elem
    else {
      val p = random.nextDouble()
      if (p <= reservoirSize.toDouble/index.toDouble)
        resevoir(random.nextInt(reservoirSize-1)) = elem
    }
    count.increment()
  }

  def results = resevoir.toList


}
