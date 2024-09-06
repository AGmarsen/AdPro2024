file:///C:/Users/thorc/OneDrive/Dokumenter/CS%201.%20Semester/Advanced%20Programming/2024-adpro/02-adt/Exercises.scala
### java.nio.file.InvalidPathException: Illegal char <:> at index 3: jar:file:///C:/Users/thorc/AppData/Local/Coursier/cache/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.12/scala-library-2.13.12-sources.jar!/scala/Tuple2.scala

occurred in the presentation compiler.

presentation compiler configuration:
Scala version: 3.3.3
Classpath:
<HOME>\AppData\Local\Coursier\cache\v1\https\repo1.maven.org\maven2\org\scala-lang\scala3-library_3\3.3.3\scala3-library_3-3.3.3.jar [exists ], <HOME>\AppData\Local\Coursier\cache\v1\https\repo1.maven.org\maven2\org\scala-lang\scala-library\2.13.12\scala-library-2.13.12.jar [exists ]
Options:



action parameters:
offset: 3391
uri: file:///C:/Users/thorc/OneDrive/Dokumenter/CS%201.%20Semester/Advanced%20Programming/2024-adpro/02-adt/Exercises.scala
text:
```scala
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
    def f (a: List[Int], b: List[Int], ac: List[Int]) : List[Int] =
      (a, b) m@@

    f(l, r, Nil)

  // Exercise 17

  def zipWith[A, B, C] (l: List[A], r: List[B], f: (A,B) => C): List[C] = ???

  // Exercise 18

  def hasSubsequence[A] (sup: List[A], sub: List[A]): Boolean = ???

```



#### Error stacktrace:

```
java.base/sun.nio.fs.WindowsPathParser.normalize(WindowsPathParser.java:182)
	java.base/sun.nio.fs.WindowsPathParser.parse(WindowsPathParser.java:153)
	java.base/sun.nio.fs.WindowsPathParser.parse(WindowsPathParser.java:77)
	java.base/sun.nio.fs.WindowsPath.parse(WindowsPath.java:92)
	java.base/sun.nio.fs.WindowsFileSystem.getPath(WindowsFileSystem.java:232)
	java.base/java.nio.file.Path.of(Path.java:147)
	java.base/java.nio.file.Paths.get(Paths.java:69)
	scala.meta.io.AbsolutePath$.apply(AbsolutePath.scala:58)
	scala.meta.internal.metals.MetalsSymbolSearch.$anonfun$definitionSourceToplevels$2(MetalsSymbolSearch.scala:70)
	scala.Option.map(Option.scala:242)
	scala.meta.internal.metals.MetalsSymbolSearch.definitionSourceToplevels(MetalsSymbolSearch.scala:69)
	scala.meta.internal.pc.completions.CaseKeywordCompletion$.scala$meta$internal$pc$completions$CaseKeywordCompletion$$$sortSubclasses(MatchCaseCompletions.scala:331)
	scala.meta.internal.pc.completions.CaseKeywordCompletion$.matchContribute(MatchCaseCompletions.scala:279)
	scala.meta.internal.pc.completions.Completions.advancedCompletions(Completions.scala:388)
	scala.meta.internal.pc.completions.Completions.completions(Completions.scala:184)
	scala.meta.internal.pc.completions.CompletionProvider.completions(CompletionProvider.scala:89)
	scala.meta.internal.pc.ScalaPresentationCompiler.complete$$anonfun$1(ScalaPresentationCompiler.scala:155)
```
#### Short summary: 

java.nio.file.InvalidPathException: Illegal char <:> at index 3: jar:file:///C:/Users/thorc/AppData/Local/Coursier/cache/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.12/scala-library-2.13.12-sources.jar!/scala/Tuple2.scala