package parser.structures

case class ModifiablePrimaryNode(
  identifier: String,
  memberAccesses: List[MemberAccess],
  arrayAccesses: List[ArrayAccess]
) extends ModifiablePrimary

case class MemberAccess(identifier: String)
case class ArrayAccess(index: Expression)