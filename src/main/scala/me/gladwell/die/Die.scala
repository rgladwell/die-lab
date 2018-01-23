package me.gladwell.die

import scala.util.Random

case class Die(range: Range)(implicit val random: Random) {

  private val high = range.end + 1
  private val low  = range.start

  def apply(): Long = random.nextInt(high - low) + low

}
