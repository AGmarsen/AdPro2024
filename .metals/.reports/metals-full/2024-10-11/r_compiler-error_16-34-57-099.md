file:///C:/Users/thorc/OneDrive/Dokumenter/CS%201.%20Semester/Advanced%20Programming/2024-adpro/07-prop/Exercises.scala
### java.lang.AssertionError: assertion failed

occurred in the presentation compiler.

presentation compiler configuration:
Scala version: 3.3.3
Classpath:
<HOME>\AppData\Local\Coursier\cache\v1\https\repo1.maven.org\maven2\org\scala-lang\scala3-library_3\3.3.3\scala3-library_3-3.3.3.jar [exists ], <HOME>\AppData\Local\Coursier\cache\v1\https\repo1.maven.org\maven2\org\scala-lang\scala-library\2.13.12\scala-library-2.13.12.jar [exists ]
Options:



action parameters:
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
    org.scalacheck.Prop.forAll(intList) {(l: List[Int]) => val min = minimum(l); l.forall(_ >= min)}

  def p2Min (minimum: List[Int] => Int): org.scalacheck.Prop = 
    org.scalacheck.Prop.forAll(intList) {(l: List[Int]) => val min = minimum(l); l.exists(_ == min)}

end Exercise_3

// Exercise 4

object Exercise_4:

  // This implementation of Prop is only used in Exercise 4
  trait Prop:
    self => // TODO remove
    def check: Boolean

    infix def && (that: Prop): Prop = 
      new Prop :
        def check: Boolean = self.check && that.check
      

end Exercise_4

opaque type Gen[+A] = State[RNG, A]

object Gen:
  
  extension [A](g: Gen[A]) 
    // Let's convert generator to streams of generators
    def toLazyList (rng: RNG): LazyList[A] =
      LazyList.unfold[A,RNG](rng)(rng => Some(g.run(rng)))

  // Exercise 5
  def choose (start: Int, stopExclusive: Int): Gen[Int] = 
    State{(s: RNG) => val (a, s2) = s.nextInt; (start + a % (stopExclusive - start), s2)}
    

  // Exercise 6

  def unit[A] (a: =>A): Gen[A] = State{(s : RNG) => (a, s)}

  def boolean: Gen[Boolean] = 
    State{(s: RNG) => val (a, s2) = s.nextInt; (a % 2 == 0, s2)}

  def double: Gen[Double] = 
    State{(s: RNG) => val (a, s2) = s.nextInt; val (b, s3) = s2.nextInt; (a / (if b != 0 then b else 1), s3)}

  // Exercise 7
  
  extension [A](self: Gen[A])

    def listOfN(n: Int): Gen[List[A]] =
      State.sequence[RNG, A](List.fill(n)(State{(s: RNG) => self.run(s)}))

  // Exercise 8
 
  // We wan't to be able to use the method on all Gen[A] and not just out Gen object.
  
  // Exercise 9

  extension [A](self: Gen[A])

    def flatMap[B](f: A => Gen[B]): Gen[B] = State.flatMap(self)(f)
      

    // It will be convenient to also have map (uses flatMap)
    def map[B](f: A => B): Gen[B] = 
      self.flatMap { a => unit(f(a)) }


  // Exercise 10

  extension [A](self: Gen[A])

    def listOf(size: Gen[Int]): Gen[List[A]] = 
      size.flatMap(n => self.flatMap {_ => self.listOfN(n)}}


  // Exercise 11

  extension [A](self: Gen[A])
    def union (that: Gen[A]): Gen[A] =
      self.flatMap(s => )

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
scala.runtime.Scala3RunTime$.assertFailed(Scala3RunTime.scala:11)
	dotty.tools.dotc.core.Types$RefinedType.<init>(Types.scala:3146)
	dotty.tools.dotc.core.Types$CachedRefinedType.<init>(Types.scala:3189)
	dotty.tools.dotc.core.Types$RefinedType$.apply(Types.scala:3198)
	dotty.tools.dotc.core.Types$ClassInfo.refineSelfType$1(Types.scala:5114)
	dotty.tools.dotc.core.Types$ClassInfo.integrateOpaqueMembers$$anonfun$1(Types.scala:5118)
	scala.collection.LinearSeqOps.foldLeft(LinearSeq.scala:183)
	scala.collection.LinearSeqOps.foldLeft$(LinearSeq.scala:179)
	scala.collection.immutable.List.foldLeft(List.scala:79)
	dotty.tools.dotc.core.Types$ClassInfo.integrateOpaqueMembers(Types.scala:5122)
	dotty.tools.dotc.typer.Namer$ClassCompleter.completeConstructor(Namer.scala:1467)
	dotty.tools.dotc.typer.Namer$ClassCompleter.completeInCreationContext(Namer.scala:1589)
	dotty.tools.dotc.typer.Namer$Completer.complete(Namer.scala:814)
	dotty.tools.dotc.core.SymDenotations$SymDenotation.completeFrom(SymDenotations.scala:174)
	dotty.tools.dotc.core.Denotations$Denotation.completeInfo$1(Denotations.scala:187)
	dotty.tools.dotc.core.Denotations$Denotation.info(Denotations.scala:189)
	dotty.tools.dotc.core.Denotations$SingleDenotation.computeAsSeenFrom(Denotations.scala:1117)
	dotty.tools.dotc.core.Denotations$SingleDenotation.computeAsSeenFrom(Denotations.scala:1070)
	dotty.tools.dotc.core.Denotations$PreDenotation.asSeenFrom(Denotations.scala:134)
	dotty.tools.dotc.core.SymDenotations$ClassDenotation.findMember(SymDenotations.scala:2170)
	dotty.tools.dotc.core.Types$Type.go$1(Types.scala:753)
	dotty.tools.dotc.core.Types$Type.findMember(Types.scala:900)
	dotty.tools.dotc.core.Types$Type.memberBasedOnFlags(Types.scala:704)
	dotty.tools.dotc.core.Types$Type.member(Types.scala:688)
	dotty.tools.dotc.typer.Checking.checkNoModuleClash(Checking.scala:962)
	dotty.tools.dotc.typer.Checking.checkNoModuleClash$(Checking.scala:809)
	dotty.tools.dotc.typer.Typer.checkNoModuleClash(Typer.scala:117)
	dotty.tools.dotc.typer.Typer.typedValDef(Typer.scala:2420)
	dotty.tools.dotc.typer.Typer.typedNamed$1(Typer.scala:3023)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3114)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3187)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3191)
	dotty.tools.dotc.typer.Typer.traverse$1(Typer.scala:3213)
	dotty.tools.dotc.typer.Typer.typedStats(Typer.scala:3259)
	dotty.tools.dotc.typer.Typer.typedClassDef(Typer.scala:2669)
	dotty.tools.dotc.typer.Typer.typedTypeOrClassDef$1(Typer.scala:3038)
	dotty.tools.dotc.typer.Typer.typedNamed$1(Typer.scala:3042)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3114)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3187)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3191)
	dotty.tools.dotc.typer.Typer.traverse$1(Typer.scala:3213)
	dotty.tools.dotc.typer.Typer.typedStats(Typer.scala:3259)
	dotty.tools.dotc.typer.Typer.typedPackageDef(Typer.scala:2812)
	dotty.tools.dotc.typer.Typer.typedUnnamed$1(Typer.scala:3083)
	dotty.tools.dotc.typer.Typer.typedUnadapted(Typer.scala:3115)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3187)
	dotty.tools.dotc.typer.Typer.typed(Typer.scala:3191)
	dotty.tools.dotc.typer.Typer.typedExpr(Typer.scala:3303)
	dotty.tools.dotc.typer.TyperPhase.typeCheck$$anonfun$1(TyperPhase.scala:44)
	dotty.tools.dotc.typer.TyperPhase.typeCheck$$anonfun$adapted$1(TyperPhase.scala:50)
	scala.Function0.apply$mcV$sp(Function0.scala:42)
	dotty.tools.dotc.core.Phases$Phase.monitor(Phases.scala:440)
	dotty.tools.dotc.typer.TyperPhase.typeCheck(TyperPhase.scala:50)
	dotty.tools.dotc.typer.TyperPhase.runOn$$anonfun$3(TyperPhase.scala:84)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.immutable.List.foreach(List.scala:333)
	dotty.tools.dotc.typer.TyperPhase.runOn(TyperPhase.scala:84)
	dotty.tools.dotc.Run.runPhases$1$$anonfun$1(Run.scala:246)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.ArrayOps$.foreach$extension(ArrayOps.scala:1323)
	dotty.tools.dotc.Run.runPhases$1(Run.scala:262)
	dotty.tools.dotc.Run.compileUnits$$anonfun$1(Run.scala:270)
	dotty.tools.dotc.Run.compileUnits$$anonfun$adapted$1(Run.scala:279)
	dotty.tools.dotc.util.Stats$.maybeMonitored(Stats.scala:71)
	dotty.tools.dotc.Run.compileUnits(Run.scala:279)
	dotty.tools.dotc.Run.compileSources(Run.scala:194)
	dotty.tools.dotc.interactive.InteractiveDriver.run(InteractiveDriver.scala:165)
	scala.meta.internal.pc.MetalsDriver.run(MetalsDriver.scala:45)
	scala.meta.internal.pc.SemanticdbTextDocumentProvider.textDocument(SemanticdbTextDocumentProvider.scala:34)
	scala.meta.internal.pc.ScalaPresentationCompiler.semanticdbTextDocument$$anonfun$1(ScalaPresentationCompiler.scala:238)
```
#### Short summary: 

java.lang.AssertionError: assertion failed