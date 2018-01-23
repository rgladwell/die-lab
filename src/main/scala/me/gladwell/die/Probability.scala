package me.gladwell.die

import scala.BigDecimal

case class Probability(p: BigDecimal) {

  require(p >= 0.0, s"probability $p was < 0.0")
  require(p <= 1.0, s"probability $p was > 1.0")

  private def toDecimalPlaces(place: Int, d: BigDecimal): Double = d.setScale(place, BigDecimal.RoundingMode.HALF_UP).toDouble

  override def toString = toDecimalPlaces(2, p).toString

}

object Probabilty {
  def apply(d: Double): Probability = Probability(BigDecimal(d))
}
