package parser

sealed trait SimpleDeclaration(declName: String, declType: Type)

case class VariableDeclaration(
    declName: String,
    declType: Type,
    expression: Expression
) extends SimpleDeclaration(declName, declType)

case class TypeDeclaration(
    declName: String,
    declType: Type
) extends SimpleDeclaration(declName, declType)

case class RoutineDeclaration(
    rtHeader: RoutineHeader,
    rtBody: Option[RoutineBody]
) 

case class RoutineHeader(
    rtName: String,
    params: Parameters,
    rtType: Type
)

case class RoutineBody(
    rtBody: Body,
    resExpression: Expression
)

case class Parameters(paramDeclarations: Vector[ParameterDeclaration])

case class ParameterDeclaration(declName: String, declType: Type)
