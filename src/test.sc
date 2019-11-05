abstract class List [+ A ]
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
    case Empty => throw new Exception( "reduceRight on empty list" )
    case Cons (h, t) => foldRight(t, h, f)
  }

  def reduceLeft [ A ](as: List [ A ], f:( A , A ) => A ): A = as match {
    case Empty => throw new Exception( "reduceLeft on empty list" )
    case Cons (h, t) => foldLeft(t, h, f)
  }
}