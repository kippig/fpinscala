package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new Exception("Empty lists do not have tails")
    case Cons(_, xs) => xs
  }

  def head[A](l: List[A]): A = l match {
    case Cons(h, _) => h
    case Nil => throw new Exception("List must not be empty")
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => List(h)
    case Cons(_, xs) => Cons(h, xs)
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
    case (_, 0) => l
    case (Nil, _) => Nil
    case (Cons(_,xs), _) =>   drop(xs, n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, xs) if f(h) => dropWhile(xs, f)
    case Cons(h, xs) => Cons(h, dropWhile(xs, f))
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new Exception("Cannot remove last member of empty List")
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]) = {
    foldRight(l, 0)( (_, y) => y+1)
  }

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
      case Cons(h, Nil) => f(z, h)
      case Cons(h,_) => foldLeft(drop(l, 1), f(z, h))(f)
      case _ => z
  }

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)(_+_)

  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_*_)

  def length3[A](ns: List[A]) =
    foldLeft(ns, 0)( (y, _) => y + 1)

  def reverse[A](ns: List[A]): List[A] = {
    foldLeft(drop(ns,1), List(head(ns)))( (x, y) => Cons(y, x))
  }

  def append_value[A](ns: List[A], z:A): List[A] = {
    foldRight(ns, Cons(z, Nil))( (a, b) => Cons(a, b))
  }

  def append2[A](xs: List[A], ys:List[A]): List[A] = {
    foldRight(xs, ys)(Cons(_, _))
  }

  def append3[A](ns: List[A], z:A): List[A] = {
    reverse(setHead(reverse(ns), z))
  }

  def concat[A](l: List[List[A]]): List[A] = {
    // foldLeft(l, Nil:List[A])( (x,y) => append2(x,y) ) This is not linear, but n^2
    foldRight(l, Nil:List[A])( (x,y) => append2(x,y))
  }

  def map[A,B](as: List[A])(f: A => B):List[B] = {
    foldRight(as, Nil:List[B])( (x,y) => Cons(f(x), y))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) Cons(x, filter(xs)(f)) else filter(xs)(f)
  }

  def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, Nil:List[A])((x,xs) => if (f(x)) Cons(x, xs) else xs)
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight(as, Nil:List[B])((x, xs) => append(f(x), xs))
  }

  def flatMap2[A,B](as: List[A])(f: A => List[B]): List[B] = {
    concat(map(as)(f))
  }

  def filter3[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap2(as)( x => if (f(x)) Cons(x,Nil) else Nil:List[A])
  }

  def zipWith[A](xs: List[A], ys: List[A])(f: (A,A) => A): List[A] = (xs, ys) match {
    case (Nil, Nil) => Nil
    case (l, Nil) => l
    case (Nil, l) => l
    case (Cons(a, as), Cons(b, bs)) => Cons(f(a,b), zipWith(as, bs)(f))
  }

  def take[A](as: List[A], n: Int): List[A] = (as, n) match {
    case (_, 0) => Nil
    case (Cons(y, ys), _) =>  Cons(y, take(ys, n-1))
    case (Nil, _) => Nil
  }

  def startsWith[A](l: List[A], k: List[A]): Boolean = (l, k) match  {
    case (Nil, Nil) => true
    case (Nil, _) => false
    case _  => k == take(l, length(k))
  }

  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    if (startsWith(sup, sub)) true else hasSubsequence(drop(sup, 1), sub)
  }

}