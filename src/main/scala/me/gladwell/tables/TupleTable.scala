package me.gladwell.tables

case class TupleTable[R, C, T](data: Map[(R, C), T]) extends Table[R, C, T] {

  val columns: Iterable[C] = data.keys.map(_._2)
  val rows: Iterable[R] = data.keys.map(_._1)

  def get(column: C, row: R): Option[T] = data.get((row, column))

}
