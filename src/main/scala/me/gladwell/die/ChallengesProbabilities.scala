package me.gladwell.die

import scala.util.Random
import System.currentTimeMillis
import Math.round
import scala.concurrent.ExecutionContext.Implicits.global
import me.gladwell.tables.RangedRowTable

object ChallengesProbabilities extends App {

  implicit val random = new Random(currentTimeMillis)

  val d20 = Die(1 to 20)

  type Difficulty = Int

  val easy: Difficulty = 10
  val medium: Difficulty = 15
  val hard: Difficulty = 20

  val dcs: Seq[Difficulty] = Seq(easy, medium, hard)

  case class Level(level: Int, profBonus: Long, maxAbilityBonus: Long) {

    def averageBonus: Long = round(maxAbilityBonus.toFloat / 2) + round(profBonus.toFloat / 2)

    override def toString = level.toString

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

case class Challenge(dc: Difficulty, level: Level) extends Function0[Result] {

  private val successTable =
    RangedRowTable[Int, Int, Int](
      Map(
        (1 to 4) -> Map(easy -> 3, medium -> 3, hard -> 2),
        (5 to 10) -> Map(easy -> 4, medium -> 3, hard -> 2),
        (11 to 16) -> Map(easy -> 8, medium -> 4, hard -> 3),
        (17 to 20) -> Map(easy -> 10, medium -> 5, hard -> 4)
      )
    )

  val successesRequired =
    successTable
      .get(dc, level.level)
      .getOrElse(sys.error(s"could not find successes for level=$level and dc=$dc"))

  private def recursiveChallenge(rolls: Seq[Long] = Seq()): Result =
    if (rolls.count(_ >= dc) >= successesRequired) Success(rolls)
    else if (rolls.count(_ < dc) >= 3) Failure(rolls)
    else recursiveChallenge(rolls :+ (d20() + level.averageBonus))

  override def apply(): Result = recursiveChallenge()

  override def toString = s"Challenge(dc = $dc, level = ${level.level})"

}

  val probabilities =
    for {
      level <- levels
      dc <- dcs
    } yield ((level, dc) -> Lab().run{ Challenge(dc, level) })

  val table = CsvTable(levels, "Level", dcs, probabilities.toMap[(Level, Difficulty), Probability])

  println(table)

}
