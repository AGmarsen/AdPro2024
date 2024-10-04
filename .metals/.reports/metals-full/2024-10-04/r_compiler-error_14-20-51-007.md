file:///C:/Users/thorc/OneDrive/Dokumenter/CS%201.%20Semester/Advanced%20Programming/2024-adpro/06-testing/Exercises.scala
### java.lang.IndexOutOfBoundsException: 0

occurred in the presentation compiler.

presentation compiler configuration:
Scala version: 3.3.3
Classpath:
<HOME>\AppData\Local\Coursier\cache\v1\https\repo1.maven.org\maven2\org\scala-lang\scala3-library_3\3.3.3\scala3-library_3-3.3.3.jar [exists ], <HOME>\AppData\Local\Coursier\cache\v1\https\repo1.maven.org\maven2\org\scala-lang\scala-library\2.13.12\scala-library-2.13.12.jar [exists ]
Options:



action parameters:
offset: 1715
uri: file:///C:/Users/thorc/OneDrive/Dokumenter/CS%201.%20Semester/Advanced%20Programming/2024-adpro/06-testing/Exercises.scala
text:
```scala
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
import scala.runtime.LazyVals
import scala.compiletime.ops.boolean

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
  List.fill(size)(0).foldRight(empty)((a, acc) => cons(@@???, () => acc))

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

    given Arbitrary[Int] = Arbitrary(genPosInt)

    forAll {(n: Int) => errorListOfSize[Boolean](n).append(cons(true, empty)).drop(n).headOption.get}

  // Exercise 8

  // Exercise 9
 
  // Exercise 10


```



#### Error stacktrace:

```
scala.collection.LinearSeqOps.apply(LinearSeq.scala:131)
	scala.collection.LinearSeqOps.apply$(LinearSeq.scala:128)
	scala.collection.immutable.List.apply(List.scala:79)
	dotty.tools.dotc.util.Signatures$.countParams(Signatures.scala:501)
	dotty.tools.dotc.util.Signatures$.applyCallInfo(Signatures.scala:186)
	dotty.tools.dotc.util.Signatures$.computeSignatureHelp(Signatures.scala:94)
	dotty.tools.dotc.util.Signatures$.signatureHelp(Signatures.scala:63)
	scala.meta.internal.pc.MetalsSignatures$.signatures(MetalsSignatures.scala:17)
	scala.meta.internal.pc.SignatureHelpProvider$.signatureHelp(SignatureHelpProvider.scala:51)
	scala.meta.internal.pc.ScalaPresentationCompiler.signatureHelp$$anonfun$1(ScalaPresentationCompiler.scala:435)
```
#### Short summary: 

java.lang.IndexOutOfBoundsException: 0