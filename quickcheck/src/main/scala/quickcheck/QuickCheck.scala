package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[Int]
    h <- oneOf(value(empty), genHeap)
  } yield insert(i, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == Math.min(a, b)
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h))==m
  }

  property("del1") = forAll { a: Int =>
    val h1 = insert(a, empty)
    val h2 = deleteMin(h1)
    isEmpty(h2)
  }

  def sorted(h: H): Boolean = {
    def sorted(h: H, min: Int): Boolean =
      isEmpty(h) || {
        val m = findMin(h)
        min <= m && sorted(deleteMin(h), m)
      }
    sorted(h, Int.MinValue)
  }

  def length(h: H): Int = if (isEmpty(h)) 0 else length(deleteMin(h)) + 1

  def toList(h: H): List[Int] = if (isEmpty(h)) Nil else findMin(h) :: toList(deleteMin(h))

  def toDuplicateList(h: H): List[Int] = if (isEmpty(h)) Nil else findMin(h) :: findMin(h) :: toDuplicateList(deleteMin(h))

  def toHeap(list: List[Int]): H = list match {
    case Nil => empty
    case n :: ns => insert(n, toHeap(ns))
  }

  def sum(h: H): Long = if (isEmpty(h)) 0 else findMin(h) + sum(deleteMin(h))

  property("sorted") = forAll { (h: H) =>
    sorted(h)
  }

  property("sorted2") = forAll { (h: H, a: Int) =>
    sorted(insert(a, h))
  }

  property("meld") = forAll { (h1: H, h2: H) =>
    val m1 = findMin(h1)
    val m2 = findMin(h2)

    findMin(meld(h1, h2)) == Math.min(m1, m2)
  }

  property("meld2") = forAll { (h: H) =>
    length(meld(h, h)) == length(h) * 2
  }

  property("sum") = forAll { ( h1: H, h2: H) =>
    sum(meld(h1, h2)) == sum(h1) + sum(h2)
  }

  property("length") = forAll { (h: H) =>
    length(h) == toList(h).size
  }

  property("length2") = forAll { (h: H) =>
    length(h) * 2 == toDuplicateList(h).size
  }

  property("convert") = forAll { (h: H) =>
    val list = toList(h)
    val h2 = toHeap(list)
    length(h) == length(h2)
  }

  property("convert2") = forAll { (h: H) =>
    val list = toDuplicateList(h)
    val h2 = toHeap(list)
    length(h) * 2 == length(h2)
  }


}
