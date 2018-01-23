package me.gladwell.die

import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.concurrent.Await
import scala.concurrent.duration._

case class Lab(tries: Int = 100000, timeout: Duration = 1 minute) {

  def run(f: () => Result)(implicit ec: ExecutionContext): Probability = {
    val eventuallyRuns = Future.sequence((1 to tries).map( _ => Future{ f() }))

    val eventualProbability =
      for {
        runs <- eventuallyRuns
      } yield {
        val successes = runs.count{ _.isInstanceOf[Success] }
        Probability(successes.toDouble / tries)
      }

    Await.result(eventualProbability, timeout)
  }

}
