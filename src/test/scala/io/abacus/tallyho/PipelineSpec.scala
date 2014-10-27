package io.abacus.tallyho

import org.scalatest.WordSpec
import org.scalatest.ShouldMatchers
import shapeless.HNil
import scala.collection.mutable.{Map =>  MMap, HashMap}

class PipelineSpec extends WordSpec with ShouldMatchers {
  // Simple WordCounter to test things

  class StringToLength() extends SimplePipeline[String,Int,Int] {
    var count = 0
    def step(elem:String) = { count+=elem.length; elem.length}
    override def result = count
  }
  class WordCounter() extends SimplePipeline[String,String,MMap[String,Int]] {
    val words = HashMap.empty[String,Int].withDefaultValue(0)
    def step(elem:String) = {
      words.put(elem,words(elem)+1)
      elem
    }
    override def result = words
  }

  class Summer() extends SimplePipeline[Int,Int, Int] {
    var sum = 0
    def step(elem:Int) = {sum+=elem; elem}
    override def result = sum
  }



  "A simple smoke test for the Pipeline framework" should {
    "count words" in {
      val a = new WordCounter()
      a.process("a")
      a.process("b")
      a.process("b")

      a.results should be (MMap("a"->1, "b"->2) ::HNil)
    }
    "count words and their total length" in {
      val a = new WordCounter()
      val b = a.alongWith(new StringToLength())
      b.process("a")
      b.process("b")
      b.process("b")

      b.results should be (MMap("a"->1, "b"->2)::3 :: HNil)
    }

    "count words length and then pipe into sum with andThen" in {
      val a = new StringToLength()
      val b = a.pipeTo(new Summer())
      b.process("a")
      b.process("bc")
      b.results should be (3 :: HNil)
    }



  }
}

