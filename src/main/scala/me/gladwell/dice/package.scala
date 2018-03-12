package me.gladwell

import scala.util.Random
import System.currentTimeMillis

package object dice {

  object Implicits {
    implicit val globalRandom = new Random(currentTimeMillis)
  }

  val d4 = Dice(1 to 4)
  val d6 = Dice(1 to 6)
  val d8 = Dice(1 to 8)
  val d10 = Dice(1 to 10)
  val d12 = Dice(1 to 12)
  val d20 = Dice(1 to 20)
  val d100 = Dice(1 to 100)

}
