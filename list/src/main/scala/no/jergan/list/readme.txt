
- Fold right: Virker ikke. Test
- _ => Cons(head, tail)

- Append: Skal være enkelt og effektivt. Bytte ut siste Nil med other?
- Reverse: Kun ett kall.

- Jari/Erlend zoom på Linux.



https://zoom.us/j/95171641603?pwd=bU5BMGpmWktxQzh4aUlWamRkaVQ4UT09

Fagdag.
https://zoom.us/j/99356721894?pwd=SG5NeS82TWdFV1I0b2VQN1paOWQzZz09

Hei. Da fikk jeg endelig tid til å programmere litt igjen. Jeg har
- Fikset pattern-matchingen til ikke å bruke "case _"
- Skrevet om append til å bruke foldRight
- Skrevet om reverse til å bruke foldLeft
- Fikset implementasjonen av foldRight (som vi diskuterte var den ikke korrekt selv om testen virket, noe som jo ble veldig klart når jeg brukte den fra append :-)
Da blir det slik:
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
         case Cons(_, tail) => 1 + tail.size
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
         case Cons(head, tail) => Cons(f(head), tail.map(f))
      }
   }

   // legg "other" til på slutten av denne lista
   def append[AA >: A](other:List[AA]):List[AA] = {
      (this, other) match {
         case (Nil, other) => other
         case (Cons(_, _), other) => foldRight(other)((a, b) => Cons(a, b))
         // alternativt for å bruke matching context i stedet for this kan man si:
         // case (Cons(head, tail), other) => Cons(head, tail).foldRight(other)((a, b) => Cons(a, b))
      }
   }

   // returnerer en ny liste vel å kalle funksjonen f for alle elementene og appende resultatene etter hverandre
   // f.eks Cons(1, Cons(2, Nil)).flatMap(a => List(a, a + 1)) == Cons(1, Cons(2, Cons(2, Cons(3, Nil))))
   def flatMap[B](f:A => List[B]):List[B] = {
      this match {
         case Nil => Nil
         case Cons(head, tail) => f(head).append(tail.flatMap(f))
      }
   }

   // returner en liste som inneholder all elementer som er 'true' for predikatet f
   def filter(f:A => Boolean):List[A] = {
      this match {
         case Nil => Nil
         case Cons(head, tail) => if (f(head)) Cons(head, tail.filter(f)) else tail.filter(f)
      }
   }

   // returnerer listen reversert
   def reverse:List[A] = {
      this match {
         case Nil => Nil

         // Ville egentlig si bare "Nil" istedet for "Cons(head, Nil).tail",
         // men da får jeg en type feil jeg ikke helt skjønner.
         case Cons(_, _) => foldLeft(Cons(head, Nil).tail)((b, a) => Cons(a, b))
         // alternativt for å bruke matching context i stedet for this kan man si:
         // case Cons(head, tail) => tail.foldLeft(Cons(head, Nil))((b, a) => Cons(a, b))
      }
   }

   // Cons(1, Cons(2, Cons(3, Nil)).foldLeft(10)(f)
   // f(f(f(10, 1), 2), 3)
   // http://upload.wikimedia.org/wikipedia/commons/5/5a/Left-fold-transformation.png
   @annotation.tailrec
   final def foldLeft[B](acc:B)(f:(B, A) => B):B = {
      this match {
         case Nil => acc
         case Cons(head, tail) => tail.foldLeft(f(acc, head))(f)
      }
   }

   // Cons(1, Cons(2, Cons(3, Nil))).foldRight(10)(f)
   // f(1, f(2, f(3, 10)))
   // http://upload.wikimedia.org/wikipedia/commons/3/3e/Right-fold-transformation.png
   final def foldRight[B](acc:B)(f:(A, B) => B):B = {
      this match {
         case Nil => acc
         case Cons(head, tail) => f(head, tail.foldRight(acc)(f))
      }
   }

   // returnerer en liste flatet ut (om det er mulig, ellers compile error)
   // f.eks. Cons(Cons(1, Nil), Cons(2, Nil)).flatten == Cons(1, Cons(2, Nil))
   def flatten[B](implicit f:A => List[B]):List[B] = {
      flatMap(f)
   }

   // returnerer summen av elementene i listen (om den inneholder nummer, ellers compile error)
   def sum[B >: A](implicit num:Numeric[B]):B = {
      foldLeft(num.zero)(num.plus)
      // eller foldRight(num.zero)(num.plus)
      // da plus er kommutativ
   }

}

For gøy (og hvis vi ser bort fra ytelse, og hvis flatten ikke i sin tur brukte append) kunne vel append også vært skrevet som
         case (Cons(head, tail), other) => Cons(Cons(head, tail), Cons(other, Nil)).flatten

