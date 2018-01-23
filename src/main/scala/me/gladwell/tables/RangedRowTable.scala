package me.gladwell.tables

case class RangedRowTable[R, C, T](data:  Map[IndexedSeq[R], Map[C, T]]) extends Table[R, C, T] {

  def get(column: C, row: R): Option[T] =
    for {
      (_, c) <- data.find{ case(range, _) => range.contains(row) }
      value <- c.get(column)
    } yield value

}
