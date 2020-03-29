package com.example

import cats._

object SetLogic {
  import cats.free.{ Free => Free_}

  sealed trait Term
  case class Literal[T](value: T) extends Term
  case class Not(value: Term) extends Term
  case class And(value: List[Term]) extends Term
  case class Or(value: List[Term]) extends Term

  // pure
  def compile(term: Term): Term = term match {
    case Not(Not(value)) => compile(value)
    case And(list: List[Term]) if list.size == 1 => compile(list.head)
    case Or(list: List[Term]) if list.size == 1 => compile(list.head)
    case value => value
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
// scala> Main.run
//
object Main {
  // Note that the pure approach is a unique in that the state is simply an Int
  // as opposed to a Counter containing an Int
  def runLiteral = SetLogic.compile(SetLogic.Literal(1))

  def runNot = SetLogic.compile(SetLogic.Not(SetLogic.Not(SetLogic.Literal(1))))

  def runRecursive = SetLogic.compile(
    SetLogic.Not(SetLogic.Not(SetLogic.Not(SetLogic.Not(SetLogic.Not(SetLogic.Literal(1))))))
  )

  def runAndSingle = SetLogic.compile(SetLogic.And(List(SetLogic.Literal(1))))

  def runOrSingle = SetLogic.compile(SetLogic.Or(List(SetLogic.Literal(1))))
}
