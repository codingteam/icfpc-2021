package org.codingteam.icfpc2021.sat_solver

case class Impossible(private val msg: String) extends Exception(msg)

sealed abstract class Expression {
  def toCNF(): Expression
}

case class Term(n: Int) extends Expression {
  def canEqual(a: Any): Boolean = a.isInstanceOf[Term]
  override def equals(that: Any): Boolean =
    that match {
      case that: Term => that.canEqual(this) && this.hashCode == that.hashCode
      case _ => false
    }
  override def hashCode: Int = n*2
  override def toString: String = n.toString

  override def toCNF(): Expression = this
}

case class And(lhs: Expression, rhs: Expression) extends Expression {
  def canEqual(a: Any): Boolean = a.isInstanceOf[And]
  override def equals(that: Any): Boolean =
    that match {
      case that: And => that.canEqual(this) && this.hashCode == that.hashCode
      case _ => false
    }
  override def hashCode: Int = 3 + lhs.hashCode*5 + rhs.hashCode*7
  override def toString: String = {
    val result = new StringBuilder()
    result.append(this.getClass.getSimpleName)
    result.append("(")
    result.append(lhs.toString)
    result.append(",")
    result.append(rhs.toString)
    result.append(")")
    result.toString
  }

  override def toCNF(): Expression =
    (lhs.toCNF(), rhs.toCNF()) match {
      case (AlwaysFalse, e) => AlwaysFalse
      case (e, AlwaysFalse) => AlwaysFalse
      case (AlwaysTrue, e) => e
      case (e, AlwaysTrue) => e

      case (a: Term, b: Term) => And(a, b)

      case (a, Not(b)) =>
        if (a == b) {
          AlwaysFalse
        } else {
          And(a, Not(b))
        }
      case (Not(a), b) =>
        if (a == b) {
          AlwaysFalse
        } else {
          And(Not(a), b)
        }

      case (a, b) =>
        if (a == b) {
          a
        } else {
          And(a, b)
        }
    }
}

case class Or(lhs: Expression, rhs: Expression) extends Expression {
  def canEqual(a: Any): Boolean = a.isInstanceOf[Or]
  override def equals(that: Any): Boolean =
    that match {
      case that: Or => that.canEqual(this) && this.hashCode == that.hashCode
      case _ => false
    }
  override def hashCode: Int = 11 + lhs.hashCode*13 + rhs.hashCode*17
  override def toString: String = {
    val result = new StringBuilder()
    result.append(this.getClass.getSimpleName)
    result.append("(")
    result.append(lhs.toString)
    result.append(",")
    result.append(rhs.toString)
    result.append(")")
    result.toString
  }

  override def toCNF(): Expression =
    (lhs.toCNF(), rhs.toCNF()) match {
      case (AlwaysFalse, e) => e
      case (e, AlwaysFalse) => e
      case (AlwaysTrue, e) => AlwaysTrue
      case (e, AlwaysTrue) => AlwaysTrue

      case (a: Term, b: Term) => Or(a, b)

      case (a, Not(b)) =>
        if (a == b) {
          AlwaysTrue
        } else {
          Or(a, Not(b))
        }
      case(Not(a), b) =>
        if (a == b) {
          AlwaysTrue
        } else {
          Or(Not(a), b)
        }

      // distributivity law
      case (a, And(x, y)) => And(Or(a, x).toCNF(), Or(a, y).toCNF()).toCNF()
      case (And(x, y), a)  => And(Or(a, x).toCNF(), Or(a, y).toCNF()).toCNF()

      case (a, b) =>
        if (a == b) {
          a
        } else {
          And(AlwaysTrue, Or(a, b))
        }
    }
}

case class Not(inner: Expression) extends Expression {
  def canEqual(a: Any): Boolean = a.isInstanceOf[Not]
  override def equals(that: Any): Boolean =
    that match {
      case that: Not => that.canEqual(this) && this.hashCode == that.hashCode
      case _ => false
    }
  override def hashCode: Int = 19 + inner.hashCode*23
  override def toString: String = {
    val result = new StringBuilder()
    result.append(this.getClass.getSimpleName)
    result.append("(")
    result.append(inner.toString)
    result.append(")")
    result.toString
  }

  override def toCNF(): Expression =
    inner.toCNF() match {
      // De Morgan's law
      case Or(a, b) => And(Not(a).toCNF(), Not(b).toCNF()).toCNF()
      case And(a, b) => Or(Not(a).toCNF(), Not(b).toCNF()).toCNF()
      // remove double negation
      case Not(e) => e
      case _ => this
    }
}

object AlwaysTrue extends Expression {
  override def toString: String = "AlwaysTrue"
  override def toCNF(): Expression = this
}

object AlwaysFalse extends Expression {
  override def toString: String = "AlwaysFalse"
  override def toCNF(): Expression = this
}

case class BooleanLogic() {
  private var expression: Expression = AlwaysTrue

  def and(expr: Expression) =
    expression =
      expression match {
        case AlwaysTrue => expr
        case e => And(e, expr)
      }

  def toCNF(): Expression = {
    expression.toCNF()
  }
}
