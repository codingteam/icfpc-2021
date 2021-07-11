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

case class And(inner: Vector[Expression]) extends Expression {
  def canEqual(a: Any): Boolean = a.isInstanceOf[And]
  override def equals(that: Any): Boolean =
    that match {
      case that: And => that.canEqual(this) && this.hashCode == that.hashCode
      case _ => false
    }
  override def hashCode: Int = 5 + inner.hashCode*11
  override def toString: String = {
    val result = new StringBuilder()
    result.append(this.getClass.getSimpleName)
    result.append("(")
    inner.addString(result, ", ")
    result.append(")")
    result.toString
  }
}
object And {
  def apply(inner: Expression*) = new And(inner.toVector)
}

case class Or(inner: Vector[Expression]) extends Expression {
  def canEqual(a: Any): Boolean = a.isInstanceOf[Or]
  override def equals(that: Any): Boolean =
    that match {
      case that: Or => that.canEqual(this) && this.hashCode == that.hashCode
      case _ => false
    }
  override def hashCode: Int = 3 + inner.hashCode*13
  override def toString: String = {
    val result = new StringBuilder()
    result.append(this.getClass.getSimpleName)
    result.append("(")
    inner.addString(result, ", ")
    result.append(")")
    result.toString
  }
}
object Or {
  def apply(inner: Expression*) = new Or(inner.toVector)
}

case class Not(inner: Vector[Expression]) extends Expression {
  def canEqual(a: Any): Boolean = a.isInstanceOf[Not]
  override def equals(that: Any): Boolean =
    that match {
      case that: Not => that.canEqual(this) && this.hashCode == that.hashCode
      case _ => false
    }
  override def hashCode: Int = 7 + inner.hashCode*17
  override def toString: String = {
    val result = new StringBuilder()
    result.append(this.getClass.getSimpleName)
    result.append("(")
    inner.addString(result, ", ")
    result.append(")")
    result.toString
  }
}
object Not {
  def apply(inner: Expression*) = new Not(inner.toVector)
}

case class BooleanLogic() {
  private var expression = And()

  def and(expr: Expression) = {
    expression =
      expr match {
        case e: And => And(expression.inner ++ e.inner)
        case _ => And(expression.inner.appended(expr))
      }
  }

  def toCNF(): Expression = {
    expression
  }
}
