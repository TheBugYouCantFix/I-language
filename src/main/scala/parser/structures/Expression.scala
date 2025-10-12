package parser.structures

import parser.*

case class Relation(
  left: Simple,
  comparisons: List[(ComparisonOperator, Simple)]
) extends Expression

case class Simple(
  left: Factor,
  operations: List[(BinaryOperator, Factor)]
) extends Expression

case class Factor(
  left: Summand,
  operations: List[(BinaryOperator, Summand)]
) extends Expression

case class Summand(
  primary: Primary,
  sign: Option[Sign],
  isNot: Boolean
) extends Expression

trait Primary extends Expression

trait LiteralValue extends Primary with Literal

case class IntegerLiteral(value: Int) extends LiteralValue
case class RealLiteral(value: Double) extends LiteralValue
case class BooleanLiteral(value: Boolean) extends LiteralValue

case class ModifiablePrimaryExpression(primary: ModifiablePrimary) extends Primary

case class RoutineCallExpression(
  identifier: String,
  arguments: List[Expression]
) extends Primary

case class ParenthesizedExpression(expression: Expression) extends Primary

// Sign for unary operations
sealed trait Sign
case object Positive extends Sign
case object Negative extends Sign

// Binary operators
sealed trait ArithmeticOperator extends BinaryOperator
case object Plus extends ArithmeticOperator
case object Minus extends ArithmeticOperator
case object Multiply extends ArithmeticOperator
case object Divide extends ArithmeticOperator
case object Modulo extends ArithmeticOperator

// Comparison operators
case object LessThan extends ComparisonOperator
case object LessThanOrEqual extends ComparisonOperator
case object GreaterThan extends ComparisonOperator
case object GreaterThanOrEqual extends ComparisonOperator
case object Equal extends ComparisonOperator
case object NotEqual extends ComparisonOperator

// Logical operators
case object And extends LogicalOperator
case object Or extends LogicalOperator
case object Xor extends LogicalOperator