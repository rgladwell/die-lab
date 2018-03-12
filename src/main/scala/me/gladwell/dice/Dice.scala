package me.gladwell.dice

import scala.util.Random

case class Dice(range: Range) {

  private val high = range.end + 1
  private val low  = range.start

  def apply()(implicit random: Random): Long = random.nextInt(high - low) + low

}
