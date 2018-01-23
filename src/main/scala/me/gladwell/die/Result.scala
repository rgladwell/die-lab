package me.gladwell.die

sealed abstract class Result

case class Success(rolls: Seq[Long]) extends Result
case class Failure(rolls: Seq[Long]) extends Result
