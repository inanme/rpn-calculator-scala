package org.inanme.expression

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FeatureSpec, GivenWhenThen}

class EvaluatorTest extends FeatureSpec with GivenWhenThen with TableDrivenPropertyChecks {

  feature("Evaluator should be able to evaluate expressions") {

    scenario("should evaluate RPN expressions") {
      val noparams = Map.empty[String, Int]

      val expressions = Table(
        ("Expression", "Expected Result", ""),
        ("2", 2, noparams),
        ("A", 2, Map("A" → 2)),
        ("4 5 +", 9, noparams),
        ("4 5 -", 1, noparams),
        ("4  --", 3, noparams),
        ("4  ++", 5, noparams),
        ("1 4 +  5 -", 0, noparams),
        ("1 A +  5 -", 2, Map("A" → 2))
      )

      forAll(expressions) {
        (expression, expectedResult, parameters) =>
          assert(Evaluator.evaluate(Parser.parseTokens(expression), parameters) == expectedResult)
      }
    }

  }
}
