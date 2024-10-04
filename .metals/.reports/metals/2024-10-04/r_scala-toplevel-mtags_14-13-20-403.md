error id: file:///C:/Users/thorc/OneDrive/Dokumenter/CS%201.%20Semester/Advanced%20Programming/2024-adpro/06-testing/Exercises.scala:[1578..1581) in Input.VirtualFile("file:///C:/Users/thorc/OneDrive/Dokumenter/CS%201.%20Semester/Advanced%20Programming/2024-adpro/06-testing/Exercises.scala", "// Advanced Programming, A. Wąsowski, IT University of Copenhagen
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
import scala.runtime.LazyVals

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

def 

def genPosInt: Gen[Int] = 
  Gen.choose(0, 100)

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

  property("Ex04.01: take(n) does not force the (n+1)st head") = 

    given Arbitrary[Int] = Arbitrary(genPosInt)

    forAll { (n: Int) => (list2lazyList(List.fill(n)(0)).append(cons(???, empty))).take(n); true}
  
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

    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])
    given Arbitrary[Int] = Arbitrary(genPosInt)

    forAll {(n: Int) => List.fill()}

  // Exercise 8

  // Exercise 9
 
  // Exercise 10

")
file:///C:/Users/thorc/OneDrive/Dokumenter/CS%201.%20Semester/Advanced%20Programming/2024-adpro/06-testing/Exercises.scala
file:///C:/Users/thorc/OneDrive/Dokumenter/CS%201.%20Semester/Advanced%20Programming/2024-adpro/06-testing/Exercises.scala:50: error: expected identifier; obtained def
def genPosInt: Gen[Int] = 
^
#### Short summary: 

expected identifier; obtained def