package org.codingteam.icfpc2021.sat_solver

import org.codingteam.icfpc2021.sat_solver.BooleanLogic

import scala.collection.mutable.{Stack, HashSet, Buffer}

case class Impossible(private val msg: String) extends Exception(msg)

object DIMACS {
  def from(logic: BooleanLogic): String = {
    val cnf = logic.toCNF()
    if (cnf == AlwaysTrue || cnf == AlwaysFalse) {
      return ""
    }

    // invariant: cnf is not AlwaysTrue nor is it AlwaysFalse

    var expressions: Stack[Expression] = Stack(cnf)
    var variables: HashSet[Int] = HashSet()
    var clauses: Buffer[String] = Buffer()

    while (!expressions.isEmpty) {
      val expr = expressions.pop()
      expr match {
        case And(a, b) => {
          expressions.push(a)
          expressions.push(b)
        }

        case Or(a, b) => {
          var clause: Buffer[String] = Buffer()

          var subexprs: Stack[Expression] = Stack(a, b)
          while (!subexprs.isEmpty) {
            val subexpr = subexprs.pop()
            subexpr match {
              case Or(a, b) => {
                subexprs.push(a)
                subexprs.push(b)
              }
              case Term(n) => {
                variables += n
                clause.addOne(s"${n}")
              }
              case Not(Term(n)) => {
                variables += n
                clause.addOne(s"-${n}")
              }
              // AlwaysTrue, ALwaysFalse, And should never occur under Or in CNF
              case _ => throw Impossible(s"Got ${subexpr}")
            }
          }

          clause.addOne("0")
          clauses.addOne(clause.mkString(" "))
        }

        case Term(n) => {
          variables += n
          clauses.addOne(s"${n} 0")
        }

        case Not(Term(n)) => {
          variables += n
          clauses.addOne(s"-${n} 0")
        }

        case _ => throw Impossible(s"Got ${expr}")
      }
    }

    val output = new StringBuilder
    output.addAll(s"p cnf ${variables.size} ${clauses.size}\n")
    clauses.addString(output, "\n")
    output.toString
  }
}
