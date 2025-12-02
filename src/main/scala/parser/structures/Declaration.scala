package parser.structures

trait SimpleDeclaration extends Declaration

case class VariableDeclaration(
  identifier: String,
  typeAnnotation: Option[Type],
  initializer: Option[Expression]
) extends SimpleDeclaration

case class TypeDeclaration(
  identifier: String,
  typeDefinition: Type
) extends SimpleDeclaration

case class RoutineDeclaration(
  header: RoutineHeader,
  body: Option[RoutineBody]
) extends Declaration

case class RoutineHeader(
  identifier: String,
  parameters: List[ParameterDeclaration],
  returnType: Option[Type]
)

case class ParameterDeclaration(
  identifier: String,
  parameterType: Type
)

sealed trait RoutineBody
case class JustRoutineBody(body: Body) extends RoutineBody
case class RoutineBodyExpression(expression: Expression) extends RoutineBody
case class StatementDeclaration(statements: List[Statement]) extends Declaration