package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[Int]
    heap <- oneOf(const(empty) , genHeap)
  } yield insert(v, heap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("minimum of the two") = forAll { (first: Int, second: Int) =>
    findMin(insert(second, insert(first, empty))) == scala.math.min(second, first)
  }

  property("insert and delete a number to an empty heap") = forAll { n: Int =>
    isEmpty(deleteMin((insert(n, empty))))
  }

  property("sorted extractor") = forAll { heap:H =>
    def takerHelper(min:Int, h: H):Boolean = {
      if (isEmpty(h)) true
      else if(min < findMin(h)) takerHelper(findMin(h), deleteMin(h)) else false
    }
    isEmpty(heap) || takerHelper(findMin(heap), deleteMin(heap))
  }

  property("minimum of the melded heaps") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == scala.math.min(findMin(h1), findMin(h2))
  }
}
