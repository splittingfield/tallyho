package io.abacus.tallyho

import shapeless._
import shapeless.ops.hlist._


trait Pipeline[-I, +M, O <: HList] { self =>
  def pipeTo[M2, O2 <: HList](next:Pipeline[M, M2, O2]):Pipeline[I,M2,O2] = {
    new Pipeline[I,M2,O2] {
      override def process(elem:I): M2 =
        next.process(self.process(elem))
      override def results = next.results
    }
  }

  def alongWith[U <: I, M2, O2 <: HList](and:Pipeline[U,M2,O2])(implicit prepend : Prepend[O, O2]):Pipeline[U,(M,M2),prepend.Out] = {
    new Pipeline[U,(M, M2),prepend.Out] {
      override def process(elem:U) = {
        val f1 = self.process(elem)
        val f2 = and.process(elem)
        (f1,f2)
      }

      override def results = {
        self.results ::: and.results
      }
    }
  }

  def process(elem: I): M
  def results: O
}

abstract class AbstractSimplePipeline[I, M, O <: HList] extends Pipeline[I, M, O] {
  def step(item: I): M
  final def process(item: I)= step(item)
}

abstract class SimplePipeline[I,M,O] extends AbstractSimplePipeline[I,M,O :: HNil] {
  def result: O
  final def results = result :: HNil
}

abstract class Transformer[I, O] extends AbstractSimplePipeline[I, O, HNil] {
  final def results = HNil
}

