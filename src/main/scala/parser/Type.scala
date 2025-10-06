package parser

import parser.*

sealed trait Type

case object ToInfere extends Type

enum PrimitiveType extends Type:
    case Integer
    case Real
    case Boolean 

sealed trait UserType extends Type

case class ArrayType(expression: Expression, arrType: Type) 

case class RecordType(varDecl: Vector[VariableDeclaration]) extends Type
