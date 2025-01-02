/* This file is empty on purpose.   It is added, and configured if you
 * wanted to add your own tests during the exam.  It is not graded and
 * should not be submitted.
 */
package adpro

import org.scalacheck.{Gen, Arbitrary, Prop}
import org.scalacheck.Prop.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.{forAll, forAllNoShrink}
import adpro.Game.*
import adpro.Game.Player.*
import adpro.Game.Move.*
import pigaro.Dist.*

object ExamSpec
  extends org.scalacheck.Properties("exam-2023-autumn"):

  property("A test that always passes (a sanity check)") = 
    forAll { (n: Int) => n == n }

  //given rng: spire.random.rng.SecureJava = spire.random.rng.SecureJava.apply 
  property("EEEE") = 
    forAll{(n:Int) => print(game.sample(10)); true}

end ExamSpec

object NullUpdatesSpecObj
  extends RL.NullUpdatesSpec(update = RL.update, "studentrl") {}
