package no.jergan.list

import scala.{List => _, Nil => _}
import scala.sys.error
import scala.language.postfixOps

/**
 * What does this class do?
 *
 * @author <a href="mailto:oyvind@jergan.no">Oyvind Jergan</a>
 */
sealed trait List[+A] {
   // returnerer om listen er tom
   def isEmpty:Boolean = {
      this match {
         case Nil => true
         case _ => false
      }
   }

   // returnerer størrelsen på listen
   def size:Int = {
      this match {
         case Nil => 0
         case _ => 1 + tail.size
      }
   }

   // henter første elementet i listen (kaster exception ved tom liste)
   def head:A = {
      this match {
         case Nil => throw new Exception("empty")
         case Cons(x, _) => x
      }
   }

   // henter halen til listen (kaster exception ved tom liste)
   def tail:List[A] = {
      this match {
         case Nil => throw new Exception("empty")
         case Cons(_, xs) => xs
      }
   }

   // returner en ny liste ved å kalle funksjonen for hvert element i lista
   def map[B](f:A => B):List[B] = {
      this match {
         case Nil => Nil
         case _ => Cons(f(head), tail.map(f))
      }
   }

   // legg "other" til på slutten av denne lista
   def append[AA >: A](other:List[AA]):List[AA] = {
      (this, other) match {
         case (Nil, Nil) => Nil
         case (Nil, _) => other
         case (_, Nil) => this
         case (_, _) => Cons(other.head, this.reverse).reverse.append(other.tail)
      }
   }


   // returnerer en ny liste vel å kalle funksjonen f for alle elementene og appende resultatene etter hverandre
   // f.eks Cons(1, Cons(2, Nil)).flatMap(a => List(a, a + 1)) == Cons(1, Cons(2, Cons(2, Cons(3, Nil))))
   def flatMap[B](f:A => List[B]):List[B] = {
      this match {
         case Nil => Nil
         case _ => f(head).append(tail.flatMap(f))
      }
   }

   // returner en liste som inneholder all elementer som er 'true' for predikatet f
   def filter(f:A => Boolean):List[A] = {
      this match {
         case Nil => Nil
         case _ => if (f(head)) Cons(head, tail.filter(f)) else tail.filter(f)
      }
   }

   // returnerer listen reversert
   def reverse:List[A] = {
      this match {
         case Nil => Nil
         case _ => {
            if (size == 1) this else Cons(tail.reverse.head, Cons(head, tail.reverse.tail.reverse).reverse)
         }
      }
   }

   // Cons(1, Cons(2, Cons(3, Nil)).foldLeft(10)(f)
   // f(f(f(10, 1), 2), 3)
   // http://upload.wikimedia.org/wikipedia/commons/5/5a/Left-fold-transformation.png
   // @annotation.tailrec
   final def foldLeft[B](acc:B)(f:(B, A) => B):B = pending

   // Cons(1, Cons(2, Cons(3, Nil))).foldRight(10)(f)
   // f(3, f(2, f(3, 10)))
   // http://upload.wikimedia.org/wikipedia/commons/3/3e/Right-fold-transformation.png
   final def foldRight[B](acc:B)(f:(A, B) => B):B = pending

   // returnerer en liste flatet ut (om det er mulig, ellers compile error)
   // f.eks. Cons(Cons(1, Nil), Cons(2, Nil)).flatten == Cons(1, Cons(2, Nil))
   def flatten[B](implicit f:A => List[B]):List[B] = pending

   // returnerer summen av elementene i listen (om den inneholder nummer, ellers compile error)
   def sum[B >: A](implicit num:Numeric[B]):B = pending

   def pending = error("pending")
}

final case class Cons[A] (x: A, xs:List[A]) extends List[A] {
}

case object Nil extends List[Nothing] {
}

   object List {

      val list = Cons(1, Cons(2, Cons(3, Nil)))
      val list2 = Cons(4, Cons(5, Nil))
      val nested = Cons(list, Cons(list2, Nil))
      val list3 = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))


      def assertEq[A](name:String, a1:A, a2: => A){
         def msg(color:String, status:String, name:String, m:String = "") = println(color + status + " :: " + name + " " + m + Console.RESET)
         try{
            if(a1 == a2)
               msg(Console.GREEN, "OK", name)
            else
               msg(Console.RED, "FAIL", name, a1 + " != " + a2)
         } catch {
            case e if e.getMessage == "pending" => println(Console.YELLOW + "PENDING :: " + name)
            case e:Throwable => msg(Console.RED, "FAIL", name, e.getMessage)
         }
      }

      def main(args: Array[String]): Unit = {

         assertEq("isEmpty", false, list.isEmpty)
         assertEq("isEmpty", true, Nil.isEmpty)
         assertEq("size", 3, list.size)
         assertEq("head", 1, list.head)
         assertEq("tail", Cons(2, Cons(3, Nil)), list.tail)
         assertEq("map", Cons(2, Cons(3, Cons(4, Nil))), list.map(1 +))
         assertEq("append", list3, list.append(list2))
         assertEq("flatMap", list3, nested.flatMap(identity))
         assertEq("filter", Cons(2, Cons(4, Nil)), list3.filter(_ % 2 == 0))
         assertEq("reverse", Cons(3, Cons(2, Cons(1, Nil))), list.reverse)
         assertEq("foldLeft", 4, list.foldLeft(10)(_ - _))
         assertEq("foldRight", -8, list.foldRight(10)(_ - _))
         assertEq("flatten", list3, nested.flatten)
         assertEq("sum", 6, list.sum)
      }

   }
