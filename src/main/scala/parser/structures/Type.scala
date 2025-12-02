package parser.structures

trait PrimitiveType extends Type

case object IntegerType extends PrimitiveType
case object RealType extends PrimitiveType
case object BooleanType extends PrimitiveType

trait UserType extends Type

case class ArrayType(
  size: Option[Expression],
  elementType: Type
) extends UserType

case class RecordType(
  fields: List[VariableDeclaration]
) extends UserType

case class TypeAlias(identifier: String) extends Type