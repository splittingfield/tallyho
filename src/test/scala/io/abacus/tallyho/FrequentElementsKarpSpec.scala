package io.abacus.tallyho

import org.scalatest.{ShouldMatchers, WordSpec}

class FrequentElementsKarpSpec extends WordSpec with ShouldMatchers {
  "The karp counter" should {
    "count a single element" in {
      val a = new FrequentElementsKarp[String](.5)
      a.process("Hello")
      a.results() should be (Array("Hello"))
    }

    "return a majority element" in {
      val a = new FrequentElementsKarp[String](.45)
      val words = List("Blah", "Goodbye","Hello", "Hello")
      words.foreach(a.process(_))
      a.results should be (Array("Hello"))
    }

    "return a majority element + extra depending on order" in {
      val a = new FrequentElementsKarp[String](.5)
      val words = List("Blah","Blah", "Goodbye","Hello","Hello", "Hello").reverse
      words.foreach(a.process(_))
      a.results should be (Array("Hello", "Blah").reverse)
    }
  }

}
