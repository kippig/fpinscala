package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r)
    case _ => 0
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(x:Int) => x
    case Branch(l:Tree[Int], r:Tree[Int]) => maximum(l) max maximum(r)
  }

 def depth[A](t: Tree[A]): Int = t match {
   case Leaf(_) => 1
   case Branch(l, r) => (depth(l) max depth(r)) + 1
 }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](t: Tree[A])( z: A => B)(f:(B,B) => B): B = t match {
    case Leaf(x) => z(x)
    case Branch(l,r) => f(fold(l)(z)(f), fold(r)(z)(f))
  }

  def size2[A](t: Tree[A]): Int = {
    fold(t)(_ => 1)(_+_)
  }

  def maximum2(t: Tree[Int]): Int = {
    fold(t)(x => x)( _ max _)
  }

  def depth2[A](t: Tree[A]): Int = {
    fold(t)(_ => 1)( (x,y) => 1 + (x max y))
  }

  def map2[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    fold(t)(a => Leaf(f(a)):Tree[B])((x:Tree[B],y:Tree[B]) => Branch(x,y))
  }


}