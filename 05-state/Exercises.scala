// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.state

import adpro.lazyList.LazyList
import adpro.lazyList.LazyList.*


trait RNG:
  /** Generate a random `Int`. We define other functions using `nextInt`. */
  def nextInt: (Int, RNG) 

object RNG:

  case class SimpleRNG(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      // `&` is bitwise AND. We use the current seed to generate a new seed.
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL 
      // The next state, which is an `RNG` instance created from the new seed. 
      val nextRNG = SimpleRNG(newSeed)
      // `>>>` is right binary shift with zero fill. 
      // The value `n` is our new pseudo-random integer.
      val n = (newSeed >>> 16).toInt 
      // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
      (n, nextRNG) 


  // Exercise 1

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (v, s) = rng.nextInt
    (Math.abs(v), s)

  // Exercise 2

  def double(rng: RNG): (Double, RNG) = 
    val (v, s) = nonNegativeInt(rng)
    (v.toDouble / Int.MaxValue, s)

  // Exercise 3
  
  // The return type is broken and needs to be fixed
  def intDouble(rng: RNG): ((Int, Double), RNG) = 
    val (v, s) = nonNegativeInt(rng)
    val (v2, s2) = double(s)
    ((v, v2), s2)

  // The return type is broken and needs to be fixed
  def doubleInt(rng: RNG): ((Double, Int), RNG) = 
    val (v, s) = nonNegativeInt(rng)
    val (v2, s2) = double(s)
    ((v2, v), s2)

  // Exercise 4

  // The return type is broken and needs to be fixed
  def ints(size: Int)(rng: RNG): (List[Int], RNG) = 
    if size < 1 then (Nil, rng)
    else 
      val (v, s) = rng.nextInt
      val (l, s2) = ints(size - 1)(s)
      (v :: l, s2)
      


  type Rand[+A] = RNG => (A, RNG)

  lazy val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt) { i => i - i % 2 }

  // Exercise 5

  lazy val double2: Rand[Double] = 
    map(nonNegativeInt)(_.toDouble / Int.MaxValue)

  // Exercise 6

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
    k => 
      val (a, s) = ra(k)
      val (b, s2) = rb(s)
      (f(a, b), s2)

  // Exercise 7

  def sequence[A](ras: List[Rand[A]]): Rand[List[A]] =
    ras.foldRight(unit(Nil))((r: Rand[A], ac:Rand[List[A]]) => 
        map2[A, List[A], List[A]](r, ac)(_ :: _)
      )


  def ints2(size: Int): Rand[List[Int]] =
    sequence(List.fill(size)(int))

  // Exercise 8

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    k => 
      val (v, s) = f(k)
      g(v)(s)


  def nonNegativeLessThan(bound: Int): Rand[Int] =
    flatMap(int)(x => unit(Math.abs(x) % bound))

end RNG

import State.*

case class State[S, +A](run: S => (A, S)):

  // Exercise 9 (methods in class State)
  // Search for the second part (sequence) below
  
  def flatMap[B](f: A => State[S, B]): State[S, B] = 
    State(s => 
      val (a, s2) = this.run(s)
      f(a).run(s2)
      )
      

  def map[B](f: A => B): State[S, B] = 
    State(s => 
      val (a, s2) = this.run(s)
      (f(a), s2)
      )
      


  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = 
    State(s => 
      val (a, s1) = this.run(s)
      val (b, s2) = sb.run(s1)
      (f(a, b), s2)
    )


object State:

  def unit[S, A](a: A): State[S, A] =
    State { s => (a, s) }

  def modify[S](f: S => S): State[S, Unit] = for
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  // Now Rand can be redefined like this (we keep it here in the State object,
  // to avoid conflict with the other Rand in RNG).
  type Rand[A] = State[RNG, A]

  // Exercise 9 (sequence, continued)
 
  def sequence[S,A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit(Nil))((sa, ac) => sa.map2[List[A], List[A]](ac)(_ :: _))

  import adpro.lazyList.LazyList

  // Exercise 10 (stateToLazyList)
  
  def stateToLazyList[S, A](s: State[S,A])(initial: S): LazyList[A] =
    val (v, s2) = s.run(initial)
    Cons(() => v, () => stateToLazyList(s)(s2))

  // Exercise 11 (lazyInts out of stateToLazyList)
  
  def lazyInts(rng: RNG): LazyList[Int] = 
    val s = State((k : RNG) => k.nextInt)
    stateToLazyList(s)(rng)
  
  import RNG.* 
  lazy val tenStrictInts: List[Int] = 
    lazyInts(SimpleRNG(42)).take(10).toList

end State
