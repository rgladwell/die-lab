package me.gladwell.die

import Math.round
import me.gladwell.dice._
import me.gladwell.tables._
import me.gladwell.tables.csv.Csv

import scala.concurrent.ExecutionContext.Implicits.global
import me.gladwell.dice.Implicits.globalRandom

object ChallengesProbabilities extends App {

  case class Difficulty(name: String, dc: Int) {
    override def toString = name
  }

  implicit object difficultyOrdering extends Ordering[Difficulty] {
    def compare(a: Difficulty, b: Difficulty) = a.dc compare b.dc
  }

  val easy: Difficulty = Difficulty("Easy", 10)
  val medium: Difficulty = Difficulty("Medium", 15)
  val hard: Difficulty = Difficulty("Hard", 20)

  val dcs: Seq[Difficulty] = Seq(easy, medium, hard)

  case class Level(level: Int, profBonus: Long, maxAbilityBonus: Long) {

    def averageBonus: Long = round(maxAbilityBonus.toFloat / 2) + round(profBonus.toFloat / 2)

    override def toString = level.toString

  }

  implicit object levelOrdering extends Ordering[Level] {
    def compare(a: Level, b: Level) = a.level compare b.level
  }

  val levels = Seq(
    Level(1, 2, 3),
    Level(2, 2, 3),
    Level(3, 2, 3),
    Level(4, 2, 3),
    Level(5, 3, 4),
    Level(6, 3, 4),
    Level(7, 3, 4),
    Level(8, 3, 4),
    Level(9, 4, 5),
    Level(10, 4, 5),
    Level(11, 4, 5),
    Level(12, 4, 5),
    Level(13, 5, 5),
    Level(14, 5, 5),
    Level(15, 5, 5),
    Level(16, 5, 5),
    Level(17, 6, 5),
    Level(18, 6, 5),
    Level(19, 6, 5),
    Level(20, 6, 5)
  )

case class Challenge(difficulty: Difficulty, level: Level) extends Function0[Result] {

  private val successTable =
    RangedRowTable[Int, Difficulty, Int](
      Map(
        (1 to 4) -> Map(easy -> 3, medium -> 3, hard -> 2),
        (5 to 10) -> Map(easy -> 4, medium -> 3, hard -> 2),
        (11 to 16) -> Map(easy -> 8, medium -> 4, hard -> 3),
        (17 to 20) -> Map(easy -> 10, medium -> 5, hard -> 4)
      )
    )

  val successesRequired =
    successTable
      .get(difficulty, level.level)
      .getOrElse(sys.error(s"could not find successes for level=$level and dc=$difficulty"))

  private def recursiveChallenge(rolls: Seq[Long] = Seq()): Result =
    if (rolls.count(_ >= difficulty.dc) >= successesRequired) Success(rolls)
    else if (rolls.count(_ < difficulty.dc) >= 3) Failure(rolls)
    else recursiveChallenge(rolls :+ (d20() + level.averageBonus))

  override def apply(): Result = recursiveChallenge()

  override def toString = s"Challenge(dc = $difficulty, level = ${level.level})"

}

  val probabilities =
    TupleTable({
      for {
        level <- levels
        dc <- dcs
      } yield ((level, dc) -> Lab().run{ Challenge(dc, level) })
    }.toMap[(Level, Difficulty), Probability])

  val table = Csv("Level", probabilities)

  println(table)

}
