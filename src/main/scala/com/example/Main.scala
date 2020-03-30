package com.example

import cats._

object SetLogic {
  import cats.free.Trampoline
  import cats.free.Trampoline._

  sealed trait Term
  case class Literal[T](value: T) extends Term
  case class Not(value: Term) extends Term
  case class And(value: List[Term]) extends Term
  case class Or(value: List[Term]) extends Term

  // pure
  def compile(term: Term): Trampoline[Term] = term match {
    case Not(Not(value)) => defer(compile(value))
    case And(list: List[Term]) if list.size == 1 => defer(compile(list.head))
    case Or(list: List[Term]) if list.size == 1 => defer(compile(list.head))
    case value => done(value)
  }
}

//
// To run these examples:
//
// shell> sbt console
//
// scala> import com.example.Main
// import com.example.Main
//
// scala> Main.runLiteral
//
object Main {
  import cats.implicits._

  // Note that the pure approach is a unique in that the state is simply an Int
  // as opposed to a Counter containing an Int
  def runLiteral = SetLogic.compile(SetLogic.Literal(1)).run

  def runNot = SetLogic.compile(SetLogic.Not(SetLogic.Not(SetLogic.Literal(1)))).run

  def runRecursive = SetLogic.compile(
    SetLogic.Not(SetLogic.Not(SetLogic.Not(SetLogic.Not(SetLogic.Not(SetLogic.Literal(1))))))
  ).run

  def runAndSingle = SetLogic.compile(SetLogic.And(List(SetLogic.Literal(1)))).run

  def runOrSingle = SetLogic.compile(SetLogic.Or(List(SetLogic.Literal(1)))).run
}
