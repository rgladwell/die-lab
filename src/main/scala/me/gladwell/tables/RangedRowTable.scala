package me.gladwell.tables

case class RangedRowTable[R, C, T](data: Map[IndexedSeq[R], Map[C, T]])(implicit colOrd: Ordering[C], rowOrd: Ordering[R]) extends Table[R, C, T] {

  override lazy val columns = data.values.map{ _.keys }.flatten.toSeq.sorted
  override lazy val rows = data.keys.flatten.toSeq.sorted

  override def get(column: C, row: R): Option[T] =
    for {
      (_, c) <- data.find{ case(range, _) => range.contains(row) }
      value <- c.get(column)
    } yield value

}
