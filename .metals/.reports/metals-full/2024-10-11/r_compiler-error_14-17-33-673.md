file:///C:/Users/thorc/OneDrive/Dokumenter/CS%201.%20Semester/Advanced%20Programming/2024-adpro/07-prop/Exercises.scala
### java.lang.IndexOutOfBoundsException: 0

occurred in the presentation compiler.

presentation compiler configuration:
Scala version: 3.3.3
Classpath:
<HOME>\AppData\Local\Coursier\cache\v1\https\repo1.maven.org\maven2\org\scala-lang\scala3-library_3\3.3.3\scala3-library_3-3.3.3.jar [exists ], <HOME>\AppData\Local\Coursier\cache\v1\https\repo1.maven.org\maven2\org\scala-lang\scala-library\2.13.12\scala-library-2.13.12.jar [exists ]
Options:



action parameters:
offset: 862
uri: file:///C:/Users/thorc/OneDrive/Dokumenter/CS%201.%20Semester/Advanced%20Programming/2024-adpro/07-prop/Exercises.scala
text:
```scala
// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.prop

import adpro.state.*

val TODO = 42

// Exercise 1

lazy val rng1: RNG = RNG.Simple(42)

// Exercise 2

lazy val (x, rng2): (Int, RNG) = rng1.nextInt
lazy val y = x

// Exercise 3

object Exercise_3:
  
  import org.scalacheck.Prop.{forAll, forAllNoShrink}
 
  // Use this generator explicitly in the two properties
  val intList = 
    org.scalacheck.Gen.listOf(org.scalacheck.Gen.choose(0, 100))
      .suchThat (_.nonEmpty)

  // The properties are put into a function taking `minimum` as argument to
  // allow the teachers to test them with different mutants of `minimum`.
  
  def p1Min (minimum: List[Int] => Int): org.scalacheck.Prop = 
    org.scalacheck.Prop.forAll(intList)(@@(l: List[Int]) => val min = minimum(l); l.foldRight(true)((e, ac) => e >= min && ac)}

  def p2Min (minimum: List[Int] => Int): org.scalacheck.Prop = 
    //property("Exist x in l: x == minimum(l)") = 
    org.scalacheck.Prop.forAll {(l: List[Int]) => val min = minimum(l); l.foldRight(false)((e, ac) => e == min || ac)}

end Exercise_3

// Exercise 4

object Exercise_4:

  // This implementation of Prop is only used in Exercise 4
  trait Prop:
    self => // TODO remove
    def check: Boolean

    infix def && (that: Prop): Prop =
      ???

end Exercise_4

opaque type Gen[+A] = State[RNG, A]

object Gen:
  
  extension [A](g: Gen[A]) 
    // Let's convert generator to streams of generators
    def toLazyList (rng: RNG): LazyList[A] =
      LazyList.unfold[A,RNG](rng)(rng => Some(g.run(rng)))

  // Exercise 5
  def choose (start: Int, stopExclusive: Int): Gen[Int] =
    ???

  // Exercise 6

  def unit[A] (a: =>A): Gen[A] = 
    ???

  def boolean: Gen[Boolean] = 
    ???

  def double: Gen[Double] = 
    ???

  // Exercise 7
  
  extension [A](self: Gen[A])

    def listOfN(n: Int): Gen[List[A]] =
      ???

  // Exercise 8
 
  // Write here ... ???
  
  // Exercise 9

  extension [A](self: Gen[A])

    def flatMap[B](f: A => Gen[B]): Gen[B] =
      ???

    // It will be convenient to also have map (uses flatMap)
    def map[B](f: A => B): Gen[B] = 
      self.flatMap { a => unit(f(a)) }


  // Exercise 10

  extension [A](self: Gen[A])
    def listOf(size: Gen[Int]): Gen[List[A]] =
      ???


  // Exercise 11

  extension [A](self: Gen[A])
    def union (that: Gen[A]): Gen[A] =
      ???

end Gen

import Gen.*

object Exercise_12:

  // Exercise 12 (type classes, givens, using, summon)
 
  // Choose one of the following templates, but note only listOfN and
  // listOf are tested (so rename if you use another than the first)

  def listOfN[A: Gen](n: Int): Gen[List[A]] =
    ???
  
  def listOfN_[A: Gen]: Int => Gen[List[A]] =
    ???
  
  def listOfN__ [A](n: Int)(using genA: Gen[A]): Gen[List[A]] =
    ???

  // Choose one of the following templates, but note only listOfN and
  // listOf are tested (so rename if you use another than the first)
  
  def listOf[A: Gen](using genInt: Gen[Int]): Gen[List[A]] =
    ???
  
  def listOf_[A] (using genInt: Gen[Int], genA: Gen[A]): Gen[List[A]] =
    ???

end Exercise_12


// (Exercise 13)
// Read the code below (and/or Section 8.1.6). You will find the
// exercise in the bottom of this fragment, marked with ??? as usual.

opaque type TestCases = Int
opaque type FailedCase = String
opaque type SuccessCount = Int
opaque type MaxSize = Int

/** The type of results returned by property testing. */
enum Result:
  case Passed
  case Falsified(failure: FailedCase, successes: SuccessCount)

  def isFalsified: Boolean = this match
    case Passed => false
    case Falsified(_, _) => true

end Result

import Result.{Passed, Falsified}

opaque type Prop = (MaxSize, TestCases, RNG) => Result

def randomLazyList[A](g: Gen[A])(rng: RNG): LazyList[A] =
  LazyList.unfold(rng)(rng => Some(g.run(rng)))

def buildMsg[A](s: A, e: Exception): String =
  s"test case: $s\n" +
  s"generated an exception: ${e.getMessage}\n" +
  s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

def forAll[A](as: Gen[A])(f: A => Boolean): Prop =
  (max, n, rng) => 
    randomLazyList(as)(rng)
      .zip(LazyList.from(0))
      .take(n)
      .map { (a, i) =>
             try if f(a) then Passed else Falsified(a.toString, i)
             catch case e: Exception => Falsified(buildMsg(a, e), i) }
      .find(_.isFalsified)
      .getOrElse(Passed)

def forAllNotSized[A] = forAll[A]
  
extension (self: Prop)
  infix def && (that: Prop): Prop = (maxSize, tcs, rng) => 
    ???
  
  infix def || (that: Prop): Prop = (maxSize, tcs, rng) =>
    ???


// Exercise 14

/** The type of generators bounded by size */
opaque type SGen[+A] = Int => Gen[A]

extension [A](self: Gen[A])
  def unsized: SGen[A] =
    ???

// Exercise 15

extension [A](self: Gen[A]) 
  def list: SGen[List[A]] =
    ???

// A sized implementation of prop, takes MaxSize to generate
// test cases of given size.  
//
// The code below also contains the `run` method for Prop - which
// provides a simple way to execute tests. Needed in the final
// exercise below.

object SGen:

  object Prop:

    def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
      (max, n, rng) =>
        val casesPerSize = (n.toInt - 1)/max.toInt + 1
        val props: LazyList[Prop] =
          LazyList.from(0)
            .take((n.toInt min max.toInt) + 1)
            .map { i => forAllNotSized(g(i))(f) }
        val prop: Prop =
          props.map[Prop](p => (max, n, rng) => 
            p(max, casesPerSize, rng)).toList.reduce(_ && _)
        prop(max, n, rng)

  extension (self: Prop)
    def run(
      maxSize: MaxSize = 100,
      testCases: TestCases = 100,
      rng: RNG = RNG.Simple(System.currentTimeMillis)): Boolean =

      self(maxSize, testCases, rng) match
      case Result.Falsified(msg, n) =>
        println(s"\u001b[34m! Falsified after $n passed tests:\n $msg [message from our Prop framework]")
        false
      case Result.Passed =>
        println(s"\u001b[34m+ OK, passed $testCases tests. [message from our Prop framework]")
        true

end SGen

// Exercise 16

// Use this generator explicitly in the two properties
val nonEmptyList: SGen[List[Int]] =  
  (n: Int) => Gen.choose(-100, 100).listOfN(n max 1)

object Exercise_16:
  
  import SGen.*

  // The properties are put into a function taking `minimum` as argument to
  // allow the teachers to test them with different mutants of `minimum`.
  
  def p1Min(minimum: List[Int] => Int): Prop = 
    ???
  
  def p2Min(minimum: List[Int] => Int): Prop = 
    ???

end Exercise_16

// vim:cc=80:conceallevel=1

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