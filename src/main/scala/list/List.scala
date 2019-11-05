package list
import sys.error

class List {

  abstract class List [+ A]
  case object Empty extends List [ Nothing ]

  case class Cons [ A ](h: A , t: List [ A ]) extends List [ A ]
  object List {

    def foldRight [ A , B ](as: List [ A ], b: B , f:( A , B ) => B ): B = as match {
      case Empty => b
      case Cons (h, t) => f(h, foldRight(t, b, f))
    }

    @scala.annotation.tailrec
    def foldLeft [ A , B ](as: List [ A ], b: B, f:( B , A ) => B ): B = as match {
      case Empty => b
      case Cons (h, t) => foldLeft(t, f(b, h), f)
    }

    def reduceRight [ A ](as: List [ A ], f:( A , A ) => A ): A = as match {
      case Empty => error( "bzzt. reduceRight on empty list" )
      case Cons (h, t) => foldRight(t, h, f)
    }

    def reduceLeft [ A ](as: List [ A ], f:( A , A ) => A ): A = as match {
      case Empty => error( "bzzt. reduceLeft on empty list" )
      case Cons (h, t) => foldLeft(t, h, f)
    }

    def sum (as: List [ Int ]): Int = {
      foldLeft(as,0,(acc:Int, e: Int ) => acc + e)
    }

    def sumR (as: List [ Int ]): Int = {
      reduceLeft(as,(x1:Int,x2:Int) => x1+x2)
    }

    def length [ A ](as: List [ A ]):Int = {
      foldLeft(as,0,(acc:Int, e:Int ) => acc + 1)
    }

    def map [ A , B ](as: List [ A ], f: A => B ): List [ B ] = {
      foldRight(as,Empty,(e:A, acc:List[B] ) => Cons(f(e), acc ))
    }

    def filter [ A ](as: List [ A ], f: A => Boolean ): List [ A ] = {
      foldRight(as,Empty,(e:A, acc:List[A] ) => {if(f(e)) Cons(e, acc) else acc} )
    }

    def concat [ A ](x: List [ A ], y: List [ A ]): List [ A ] = {
      foldRight(x,y,(e:A, acc:List[A]) => Cons(e, acc ))
    }

    def flatten [ A ](as: List [ List [ A ]]): List [ A ] = {
      reduceLeft(as,(l1:List[A],l2:List[A]) => concat(l1,l2))
    }

    def flatMap [ A , B ](as: List [ A ], f: A => List [ B ]): List [ B ] = {
      flatten(map(as,f))
    }

    def maximum (as: List [ Int ]): Int = {
      foldLeft(as,Int.MinValue,(e:Int, acc:Int ) => if(e > acc) e else acc )
    }

    def minimum (as: List [ Int ]): Int = {
      foldLeft(as,Int.MaxValue,(e:Int, acc:Int ) => if(e < acc) e else acc )
    }

    def maximumR (as: List [ Int ]): Int = {
      reduceLeft(as,(x1,x2) => if(x1 > x2) x1 else x2 )
    }

    def reverse [ A ](as: List [ A ]): List [ A ] = {
      foldLeft(as,Empty,(acc:List[A], e:A) => Cons(e,acc))
    }
  }
}

