package no.jergan.list

import org.scalatest._
import org.scalatest.funsuite.AnyFunSuite

/**
 * Unit test of [[List]].
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
class ListTest extends AnyFunSuite {

   val list = Cons(1, Cons(2, Cons(3, Nil)))
   val list2 = Cons(4, Cons(5, Nil))
   val nested = Cons(list, Cons(list2, Nil))
   val list3 = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))

   test("isEmpty") {
      assert(!list.isEmpty);
      assert(Nil.isEmpty)
   }

   test("size") {
      assert(list.size == 3)
   }

   test("head") {
      assert(list.head == 1)
   }

   test("tail") {
      assert(list.tail == Cons(2, Cons(3, Nil)))
   }

   test("map") {
      assert(list.map(_ + 1) == Cons(2, Cons(3, Cons(4, Nil))))
   }

   test("flatMap") {
      assert(nested.flatMap(identity) == list3)
   }

   test("filter") {
      assert(list3.filter(_ % 2 == 0) == Cons(2, Cons(4, Nil)))
   }

   test("reverse") {
      assert(list.reverse == Cons(3, Cons(2, Cons(1, Nil))), list.reverse)
   }

   test("foldLeft") {
      assert(list.foldLeft(10)(_ - _) == 4)
   }

   test("foldRight") {
      assert(list.foldRight(10)(_ - _) == -8)
   }

   test("flatten") {
      assert(nested.flatten == list3)
   }

   test("sum") {
      assert(list.sum == 6)
   }

}