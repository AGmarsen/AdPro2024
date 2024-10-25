// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.adt

import java.util.NoSuchElementException

enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])


object List: 

  def head[A] (l: List[A]): A = l match
    case Nil => throw NoSuchElementException() 
    case Cons(h, _) => h                                                                                                                                                                                                                                       
  
  def apply[A] (as: A*): List[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  def append[A] (l1: List[A], l2: List[A]): List[A] =
    l1 match
      case Nil => l2
      case Cons(h, t) => Cons(h, append(t, l2)) 

  def foldRight[A, B] (l: List[A], z: B, f: (A, B) => B): B = l match
    case Nil => z
    case Cons(a, as) => f(a, foldRight(as, z, f))
    
  def map[A, B] (l: List[A], f: A => B): List[B] =
    foldRight[A, List[B]] (l, Nil, (a, z) => Cons(f(a), z))

  // Exercise 1 (is to be solved without programming)

  // Exercise 2

  def tail[A] (l: List[A]): List[A] = l match
    case Nil => throw NoSuchElementException()
    case Cons(_, t) => t
  

  // Exercise 3
  @annotation.tailrec
  def drop[A] (l: List[A], n: Int): List[A] = (l, n) match
    case (l, n) if n <= 0 => l
    case (Nil, _) => throw NoSuchElementException()
    case (Cons(_, t), n) => drop(t, n-1)

  // Exercise 4

  @annotation.tailrec
  def dropWhile[A] (l: List[A], p: A => Boolean): List[A] = l match
    case Nil => Nil
    case Cons(h, t) if p(h) => dropWhile(t, p)
    case _ => l

  // Exercise 5
 
  def init[A] (l: List[A]): List[A] = l match
    case Nil => throw NoSuchElementException()
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  

  // Exercise 6

  def length[A] (l: List[A]): Int = foldRight(l, 0, (_, B) => 1 + B)
  

  // Exercise 7

  @annotation.tailrec
  def foldLeft[A, B] (l: List[A], z: B, f: (B, A) => B): B = l match
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h), f)

  // Exercise 8

  def product (as: List[Int]): Int = foldLeft(as, 1, _ * _)

  def length1[A] (as: List[A]): Int = foldLeft(as, 0, (b, _) => b + 1)

  // Exercise 9

  def reverse[A] (l: List[A]): List[A] = foldLeft(l, Nil, (a, h) => Cons(h, a))
 
  // Exercise 10

  def foldRight1[A, B] (l: List[A], z: B, f: (A, B) => B): B = foldLeft(reverse(l), z, (B, A) => f(A, B))

  // Exercise 11

  def foldLeft1[A, B] (l: List[A], z: B, f: (B, A) => B): B = ???
 
  // Exercise 12

  def concat[A] (l: List[List[A]]): List[A] = foldRight(l, Nil, append)
  
  
  // Exercise 13

  def filter[A] (l: List[A], p: A => Boolean): List[A] = 
    foldRight(l, Nil, (a, b) => if p(a) then Cons(a, b) else b)
 
  // Exercise 14

  def flatMap[A,B] (l: List[A], f: A => List[B]): List[B] = foldRight(l, Nil, (a, b) => append(f(a), b))

  // Exercise 15

  def filter1[A] (l: List[A], p: A => Boolean): List[A] = flatMap(l, a => if p(a) then Cons(a, Nil) else Nil)

  // Exercise 16

  def addPairwise (l: List[Int], r: List[Int]): List[Int] = 
    @annotation.tailrec
    def f (a: List[Int], b: List[Int], c: List[Int] => List[Int]) : List[Int] = (a, b) match
      case (Nil, x) => c(Nil)
      case (x, Nil) => c(Nil)
      case (Cons(h1, t1), Cons(h2, t2)) => f(t1, t2, x => c(Cons(h1 + h2, x)))

    f(l, r, identity)

  // Exercise 17

  def zipWith[A, B, C] (l: List[A], r: List[B], f: (A,B) => C): List[C] = 
    @annotation.tailrec
    def aux (a: List[A], b: List[B], c: List[C] => List[C]) : List[C] = (a, b) match
      case (Nil, x) => c(Nil)
      case (x, Nil) => c(Nil)
      case (Cons(h1, t1), Cons(h2, t2)) => aux(t1, t2, x => c(Cons(f(h1, h2), x)))

    aux(l, r, identity)

  // Exercise 18

  def hasSubsequence[A] (sup: List[A], sub: List[A]): Boolean = 
    def hasPrefix (l: List[A], pref: List[A]) : Boolean = (l, pref) match 
      case (_, Nil) => true
      case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => hasPrefix(t1, t2)
      case _ => false

    (sup, sub) match
      case (_, Nil) => true
      case (Nil, _) => false
      case (s1, s2) if hasPrefix(s1, s2) => true
      case (Cons(_, t), s) => hasSubsequence(t, s)
