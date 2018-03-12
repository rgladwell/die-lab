package me.gladwell.tables

trait Table[-R, -C, +T] {

  def get(column: C, row: R): Option[T]

}
