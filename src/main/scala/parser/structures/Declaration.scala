package parser.structures

/**
 * Simple declaration (variable or type declaration)
 */
trait SimpleDeclaration extends Declaration

/**
 * Variable declaration
 */
case class VariableDeclaration(
  identifier: String,
  typeAnnotation: Option[Type],
  initializer: Option[Expression]
) extends SimpleDeclaration

/**
 * Type declaration
 */
case class TypeDeclaration(
  identifier: String,
  typeDefinition: Type
) extends SimpleDeclaration

/**
 * Routine declaration
 */
case class RoutineDeclaration(
  header: RoutineHeader,
  body: Option[RoutineBody]
) extends Declaration

/**
 * Routine header containing name, parameters, and return type
 */
case class RoutineHeader(
  identifier: String,
  parameters: List[ParameterDeclaration],
  returnType: Option[Type]
)

/**
 * Parameter declaration for routines
 */
case class ParameterDeclaration(
  identifier: String,
  parameterType: Type
)

/**
 * Routine body containing local declarations and statements
 */
case class RoutineBody(
  declarations: List[SimpleDeclaration],
  statements: List[Statement],
  returnExpression: Option[Expression]
)

/**
 * Statement declaration (for top-level statements)
 */
case class StatementDeclaration(statements: List[Statement]) extends Declaration