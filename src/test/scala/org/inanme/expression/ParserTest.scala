package org.inanme.expression

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FeatureSpec, GivenWhenThen}

class ParserTest extends FeatureSpec with GivenWhenThen with TableDrivenPropertyChecks {

  feature("Parser should be able to parse tokens") {

    scenario("should parse constants") {
      val expressions = Table(
        ("Token", "Expression"),
        ("1", Constant(1)),
        ("123", Constant(123))
      )

      forAll(expressions) {
        (token, expression) =>
          assert(Parser.parseToken(token) == expression)
      }
    }

    scenario("should parse variables") {
      val expressions = Table(
        ("Token", "Expression"),
        ("a", Variable("a")),
        ("ab", Variable("ab"))
      )

      forAll(expressions) {
        (token, expression) =>
          assert(Parser.parseToken(token) == expression)
      }
    }

    scenario("should parse operators") {
      val expressions = Table(
        ("Token", "Expression"),
        ("+", Operator("+")),
        ("++", Operator("++")),
        ("-", Operator("-")),
        ("--", Operator("--")),
        ("/", Operator("/")),
        ("*", Operator("*"))
      )

      forAll(expressions) {
        (token, expression) =>
          assert(Parser.parseToken(token) == expression)
      }
    }


    scenario("should parse list of tokens") {
      val fixture = Table(
        ("Token", "Expression"),
        ("3 1  x   +  -", List(Constant(3), Constant(1), Variable("x"), Operator("+"), Operator("-")))
      )

      forAll(fixture) {
        (token, expressions) =>
          assert(Parser.parseTokens(token) sameElements expressions)
      }
    }

  }
}
