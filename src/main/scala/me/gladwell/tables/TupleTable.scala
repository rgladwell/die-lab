package me.gladwell.tables

case class TupleTable[R, C, T](data: Map[(R, C), T])(implicit colOrd: Ordering[C], rowOrd: Ordering[R]) extends Table[R, C, T] {

  override lazy val columns = data.keys.map(_._2).toSeq.sorted
  override lazy val rows = data.keys.map(_._1).toSeq.sorted

  override def get(column: C, row: R): Option[T] = data.get((row, column))

}
