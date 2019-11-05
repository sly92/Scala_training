package tree

import scala.annotation.tailrec
import scala.sys.error

// sealed => extends only in this file
sealed trait Tree
case class Leaf(value:Int) extends Tree
case class Branch(value:Int, left:Tree, right:Tree) extends Tree

object Tree {

  def size1(t:Tree): Int = {
    if (t.isInstanceOf[Leaf])  1
    else {
      size1(t.asInstanceOf[Branch].left) + size1(t.asInstanceOf[Branch].right) + 1
    }
  }

  def size(t:Tree): Int = {
    t match {
      case l: Leaf => 1
      case b: Branch => size(b.left) + size(b.right) + 1
    }
  }

  def max(t: Tree): Int = {
    t match {
      case l: Leaf => l.value
      case b: Branch =>  max(b.left) max + max(b.right) max b.value
    }
  }

  def min(t: Tree): Int = {
    t match {
      case l: Leaf => l.value
      case b: Branch =>  max(b.left) min min(b.right) min b.value
    }
  }

  def depth(t: Tree): Int = {
    t match {
      case l: Leaf => 1
      case b: Branch =>  depth(b.left) + 1 max depth(b.right) + 1
    }
  }

 def isBalanced(t:Tree, countR:Int, countL:Int): Boolean = {
   t match {
     case l: Leaf => if (countR == countL) true else false
     case b: Branch => isBalanced(b.left, countR, countL + 1) == isBalanced(b.right, countR + 1, countL)
   }
 }

  def isSymmetric(t:Tree, depthValue:Int): Int = {
    t match {
      case l: Leaf => depthValue
      case b: Branch => depth(t)
    }
  }

    def nbLeaves(t: Tree): Int = {
      t match {
        case l: Leaf => 1
        case b: Branch => nbLeaves(b.left) + nbLeaves(b.right)
      }
    }

    def leaves(t: Tree): List[Leaf] = {
      t match {
        case l: Leaf => List(l)
        case b: Branch => leaves(b.left) ++ leaves(b.right)
      }
    }
  }

