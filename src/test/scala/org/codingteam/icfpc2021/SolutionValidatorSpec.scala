package org.codingteam.icfpc2021

import org.scalatest._
import flatspec._
import matchers._
import java.math.BigInteger

import org.codingteam.icfpc2021.{Problem, Solution, Json}
import org.codingteam.icfpc2021.validator._

class SolutionValidatorSpec extends AnyFlatSpec with should.Matchers {
  "SolutionValidator" should "accept the Lambdaman solution from the spec" in {
    val problem = Json.parseProblem(
      """{"hole":[[45,80],[35,95],[5,95],[35,50],[5,5],[35,5],[95,95],[65,95],[55,80]],"epsilon":150000,"figure":{"edges":[[2,5],[5,4],[4,1],[1,0],[0,8],[8,3],[3,7],[7,11],[11,13],[13,12],[12,18],[18,19],[19,14],[14,15],[15,17],[17,16],[16,10],[10,6],[6,2],[8,12],[7,9],[9,3],[8,9],[9,12],[13,9],[9,11],[4,8],[12,14],[5,10],[10,15]],"vertices":[[20,30],[20,40],[30,95],[40,15],[40,35],[40,65],[40,95],[45,5],[45,25],[50,15],[50,70],[55,5],[55,25],[60,15],[60,35],[60,65],[60,95],[70,95],[80,30],[80,40]]}}""",
    )
    val solution = Json.parseSolution(
      """{"vertices":[[21,28],[31,28],[31,87],[29,41],[44,43],[58,70],[38,79],[32,31],[36,50],[39,40],[66,77],[42,29],[46,49],[49,38],[39,57],[69,66],[41,70],[39,60],[42,25],[40,35]]}""",
    )

    val validator = new SolutionValidator(problem)

    validator.validate(solution) shouldBe true
  }

  it should "accept an obvious solution to the 11th problem" in {
    val problem = Json.parseProblem(
      """{"hole":[[10,0],[10,10],[0,10]],"epsilon":0,"figure":{"edges":[[0,1],[1,2],[2,0]],"vertices":[[0,0],[10,0],[10,10]]}}""",
    )
    val solution =
      Json.parseSolution("""{"vertices":[[10,0],[10,10],[0,10]]}""")

    val validator = new SolutionValidator(problem)

    validator.validate(solution) shouldBe true
  }

  it should "accept an obvious solution to the 14th problem" in {
    val problem = Json.parseProblem(
      """{"hole":[[13,0],[11,2],[14,2],[14,4],[12,9],[12,4],[10,5],[10,10],[0,0]],"epsilon":0,"figure":{"edges":[[0,1],[1,2],[2,0]],"vertices":[[0,8],[6,0],[14,6]]}}""",
    )
    val solution = Json.parseSolution("""{"vertices":[[0,0],[10,0],[10,10]]}""")

    val validator = new SolutionValidator(problem)

    validator.validate(solution) shouldBe true
  }
}
