package me.gladwell.tables

trait Table[R, C, +T] {

  val rows: Seq[R]
  val columns: Seq[C]

  def get(column: C, row: R): Option[T]

}
