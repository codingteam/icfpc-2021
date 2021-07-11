package org.codingteam.icfpc2021.sat_solver

sealed abstract class Expression

case class Term(n: Int) extends Expression {
  def canEqual(a: Any): Boolean = a.isInstanceOf[Term]
  override def equals(that: Any): Boolean =
    that match {
      case that: Term => that.canEqual(this) && this.hashCode == that.hashCode
      case _ => false
    }
  override def hashCode: Int = n*2
  override def toString: String = n.toString
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
}

object AlwaysTrue extends Expression

object AlwaysFalse extends Expression

case class BooleanLogic() {
  private var expression: Expression = AlwaysTrue

  def and(expr: Expression) =
    expression =
      expression match {
        case AlwaysTrue => expr
        case e => And(e, expr)
      }

  def toCNF(): Expression = {
    expression
  }
}
