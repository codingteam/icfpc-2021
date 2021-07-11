package org.codingteam.icfpc2021.sat_solver

class Term(n: Int) {
  def canEqual(a: Any): Boolean = a.isInstanceOf[Term]
  override def equals(that: Any): Boolean =
    that match {
      case that: Term => that.canEqual(this) && this.hashCode == that.hashCode
      case _ => false
    }
  override def hashCode: Int = n
}

class Clause(terms: Vector[Term]) {
  def canEqual(a: Any): Boolean = a.isInstanceOf[Clause]
  override def equals(that: Any): Boolean =
    that match {
      case that: Clause => that.canEqual(this) && this.hashCode == that.hashCode
      case _ => false
    }
  override def hashCode: Int = terms.hashCode
}

class Expression(clauses: Vector[Clause]) {
  def canEqual(a: Any): Boolean = a.isInstanceOf[Expression]
  override def equals(that: Any): Boolean =
    that match {
      case that: Expression => that.canEqual(this) && this.hashCode == that.hashCode
      case _ => false
    }
  override def hashCode: Int = clauses.hashCode
}

class BooleanLogic {
  def toCNF(): Expression = {
    new Expression(Vector())
  }
}
