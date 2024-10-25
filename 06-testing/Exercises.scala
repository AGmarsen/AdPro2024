// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.lazyList

import org.scalacheck.*
import org.scalacheck.Prop.*
import org.scalacheck.Arbitrary.arbitrary

 import lazyList00.* // uncomment to test the book laziness solution implementation
// import lazyList01.* // uncomment to test the broken headOption implementation
// import lazyList02.* // uncomment to test another version

/* Generators and helper functions */

import LazyList.*

/** Convert a strict list to a lazy-list */
def list2lazyList[A](la: List[A]): LazyList[A] = 
  LazyList(la*)

/** Generate finite non-empty lazy lists */
def genNonEmptyLazyList[A](using Arbitrary[A]): Gen[LazyList[A]] =
  for la <- arbitrary[List[A]].suchThat { _.nonEmpty }
  yield list2lazyList(la)
  
/** Generate an infinite lazy list of A values.
  *
  * This lazy list is infinite if the implicit generator for A never fails. The
  * code is ugly-imperative, but it avoids stack overflow (as Gen.flatMap is
  * not tail recursive)
  */
def infiniteLazyList[A: Arbitrary]: Gen[LazyList[A]] =
  def loop: LazyList[A] =
    summon[Arbitrary[A]].arbitrary.sample match
      case Some(a) => cons(a, loop)
      case None => empty
  Gen.const(loop)

def infiniteErrorList[A: Arbitrary]: Gen[LazyList[A]] =
  def loop: LazyList[A] =
    summon[Arbitrary[A]].arbitrary.sample match
      case Some(a) => cons(???, loop)
      case None => empty
  Gen.const(loop)

def errorListOfSize[A](size: Int) : LazyList[A] = 
  List.fill(size)(0).foldRight(empty)((_, acc) => cons(???, acc))

def lightAtTheEndOfTheTunnel(size: Int) : LazyList[Boolean] = //list of error with a non-error at the end
  List.fill(size)(0).foldRight(cons(true, empty))((_, acc) => cons(???, acc))

def darkAtTheEndOfTheTunnel(size: Int) : LazyList[Boolean] = //list of non-error with an error at the end
  List.fill(size)(0).foldRight(cons(???, empty))((_, acc) => cons(true, acc))

def genPosInt: Gen[Int] = 
  Gen.choose(0, 1000)

/* The test suite */

object LazyListSpec 
  extends org.scalacheck.Properties("testing"):

  // Exercise 1

  property("Ex01.01: headOption returns None on an empty LazyList") = 
    empty.headOption == None

  property("Ex01.02: headOption returns the head of the stream packaged in Some") =

    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])

    forAll { (n: Int) => cons(n,empty).headOption == Some(n) } :| "singleton" &&
    forAll { (s: LazyList[Int]) => s.headOption != None }      :| "random" 

  // Exercise 2

  property("Ex02.01: headOption does not force the tail of a lazy list") =

    forAll { (n: Int) => cons(n, ???).headOption == Some(n) }
  
  // Exercise 3

  property("Ex03.01: take does not force any heads nor any tails of the lazy list") =

    given Arbitrary[LazyList[Int]] = Arbitrary(infiniteErrorList[Int])
    given Arbitrary[Int] = Arbitrary(genPosInt)
  
    forAll { (s: LazyList[Int], n: Int) => s.take(n); true }

  // Exercise 4

  property("Ex04.01: take(n) does not force the (n+1)st head") = //this is kind of covered by Ex03

    given Arbitrary[Int] = Arbitrary(genPosInt)

    forAll { (n: Int) => darkAtTheEndOfTheTunnel(n).take(n).toList; true}
  
  // Exercise 5

  property("Ex05.01: l.take(n).take(n) == l.take(n)") =

    given Arbitrary[LazyList[Int]] = Arbitrary(infiniteLazyList[Int])
    given Arbitrary[Int] = Arbitrary(genPosInt)

    forAll { (s: LazyList[Int], n: Int) => s.take(n).take(n).toList == s.take(n).toList }
  
  // Exercise 6

  property("Ex06.01: l.drop(n).drop(m) == l.drop(n+m)") =

    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])
    given Arbitrary[Int] = Arbitrary(genPosInt)

    forAll { (s: LazyList[Int], n: Int, m: Int) => s.drop(n).drop(m).toList == s.drop(n + m).toList }
  
  // Exercise 7

  property("Ex07.01: l.drop(n) does not force any of the dropped elements") =

    given Arbitrary[Int] = Arbitrary(genPosInt)

    forAll { (n: Int) => lightAtTheEndOfTheTunnel(n).drop(n).toList.head }

  // Exercise 8

  property("Ex08.01: l.map(identity) == l") =

    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])

    forAll { (s: LazyList[Int]) => s.map(identity).toList == s.toList }

  // Exercise 9

  property("Ex09.01: map terminates on infinite lazy lists") =

    given Arbitrary[LazyList[Int]] = Arbitrary(infiniteLazyList[Int])

    forAll { (s: LazyList[Int]) => s.map(identity); true }
 
  // Exercise 10

  property("Ex10.01: l.append(empty) == l") =
    
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])

    forAll { (s: LazyList[Int]) => s.append(empty).toList == s.toList } :| "s.append(empty)" &&
    forAll { (s: LazyList[Int]) => empty.append(s).toList == s.toList } :| "empty.append(s)"

  property("Ex10.02: l.append(l2).size == l.size + l2.size") =
    
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])

    forAll { (s: LazyList[Int], s2: LazyList[Int]) => s.append(s2).toList.size == s.toList.size + s2.toList.size }

  property("Ex10.03: l.append(l2) != l2.append(l) unless l == l2 for non-empty l and l2") =
    
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])

    forAll { (s: LazyList[Int], s2: LazyList[Int]) => s.toList == s2.toList || s.append(s2).toList != s2.append(s).toList }
  
  property("Ex10.04: l.append(l2).take(k) == l if k == l.size ") =
    
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])

    forAll { (s: LazyList[Int], s2: LazyList[Int]) => s.append(s2).take(s.toList.size).toList == s.toList }

  property("Ex10.05: l.append(l2).drop(k) == l2 if k == l.size ") =
    
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])

    forAll { (s: LazyList[Int], s2: LazyList[Int]) => s.append(s2).drop(s.toList.size).toList == s2.toList }

