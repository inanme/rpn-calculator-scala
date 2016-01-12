package org.inanme.expression

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FeatureSpec, GivenWhenThen}

class ExpressionParserTest extends FeatureSpec with GivenWhenThen with TableDrivenPropertyChecks {

  feature("ExpressionParser should be able to evaluate expressions") {

    scenario("should evaluate RPN expressions") {
      val mazeDataTable = Table(
        ("Expression", "Expected Result", "Supplied Parameters"),
        ("RPN: A", 2D, Map("A" -> 2D)),
        ("RPN: 2", 2D, Map()),
        ("RPN: 4 5 *", 20D, Map()),
        ("RPN: 4 2 /", 2D, Map()),
        ("RPN: A B / 2 +", 7D, Map("A" -> 10D, "B" -> 2D)),
        ("RPN: B ++", 7D, Map("B" -> 6D)),
        ("RPN: B --", 4D, Map("B" -> 5D))
      )

      forAll(mazeDataTable) {
        (expression, expectedResult, parameters) =>
          assert(false)
      }
    }

  }
}
