package parser.structures

case class Assignment(
  target: ModifiablePrimary,
  value: Expression
) extends Statement

case class RoutineCall(
  identifier: String,
  arguments: List[Expression]
) extends Statement

case class WhileLoop(
  condition: Expression,
  body: Body
) extends Statement

case class ForLoop(
  loopVariable: String,
  range: Range,
  isReverse: Boolean,
  body: Body
) extends Statement

case class IfStatement(
  condition: Expression,
  thenBody: Body,
  elseBody: Option[Body]
) extends Statement

case class PrintStatement(
  values: List[Expression]
) extends Statement

case class Range(
  start: Expression,
  end: Option[Expression]
)

case class Body(
  declarations: List[SimpleDeclaration],
  statements: List[Statement]
)