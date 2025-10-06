package parser

sealed trait Statement

case class Assignment(
    varName: String,
    value: Expression
) extends Statement

case class RoutineCall(
    rtName: String,
    params: Vector[Expression]
) extends Statement

case class WhileLoop(
    cond: Expression,
    body: Body
) extends Statement

case class ForLoop(
    forIterVarName: String,
    range: Range,
    reverse: Boolean,
    body: Body
) extends Statement

case class Range(
    startExpression: Expression,
    endExpression: Option[Expression]
) extends Statement

case class IfStatement(
    ifExpression: Expression,
    thenBody: Body,
    elseBody: Body
) extends Statement

case class PrintStatement(
    toPrintExpressions: Vector[Expression]
) extends Statement
