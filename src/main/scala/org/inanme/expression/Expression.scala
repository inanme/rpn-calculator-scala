package org.inanme.expression

import java.util.EmptyStackException

import scala.annotation.tailrec

sealed trait Token

case class Constant(t: Int) extends Token

case class Variable(name: String) extends Token

case class Operator(name: String) extends Token

case object InvalidToken extends Token

object Parser {
  val constExpr = "(\\d+)".r
  val varExpr = "([A-Za-z]+)".r
  val opExpr = "(\\+{1,2}|-{1,2}|\\*|/)".r

  def parseTokens(token: String): List[Token] = {
    token.split("\\s+").toList map parseToken
  }

  def parseToken(token: String): Token = {
    token match {
      case opExpr(it) ⇒ Operator(it)
      case constExpr(it) ⇒ Constant(it.toInt)
      case varExpr(it) ⇒ Variable(it)
      case _ ⇒ InvalidToken
    }
  }
}

object Evaluator {

  def evaluate(exp: List[Token], parameters: Map[String, Int]): Int = {
    @tailrec
    def evaluate(exp: List[Token]): Int = {
      exp match {
        case Nil ⇒ throw new EmptyStackException
        case InvalidToken :: rest ⇒ throw new UnsupportedOperationException
        case Constant(it) :: Nil ⇒ it
        case Variable(it) :: rest ⇒ evaluate(Constant(parameters(it)) :: rest)
        case Constant(opr1) :: Variable(var1) :: Operator(op) :: rest ⇒
          evaluate(Constant(opr1) :: Constant(parameters(var1)) :: Operator(op) :: rest)
        case Constant(opr1) :: Constant(opr2) :: Operator("+") :: rest ⇒ evaluate(Constant(opr1 + opr2) :: rest)
        case Constant(opr1) :: Constant(opr2) :: Operator("-") :: rest ⇒ evaluate(Constant(opr2 - opr1) :: rest)
        case Constant(opr1) :: Operator("++") :: rest ⇒ evaluate(Constant(opr1 + 1) :: rest)
        case Constant(opr1) :: Operator("--") :: rest ⇒ evaluate(Constant(opr1 - 1) :: rest)
        case _ ⇒ throw new RuntimeException("can not evaluate:" + exp)
      }
    }
    evaluate(exp)
  }

}
