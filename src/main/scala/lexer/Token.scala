package lexer

enum Token:
  case Identifier

  // predefined types
  case Integer
  case Real
  case Boolean

  // user defined types
  case ArrayType
  case RecordType

  // symbols
  case Colon
  case LeftParen
  case RightParen

  // arithmetic operations
  case Plus
  case Minus
  case Mul
  case Div
  case Mod

  // logic operations
  case And
  case Or
  case Xor
  case Not

  // Relations
  case Eq
  case Neq
  case Lteq
  case Lt
  case Gt
  case Gteq

  // loops
  case For
  case While
  case Reverse
  case Loop

  // if - else
  case If
  case Then
  case Else

  // routine (function)
  case Routine
  case Return

  // Declarations
  case Var
  case Type

  // miscellaneous
  case Declaration // is
  case Assignment // :=
  case End
  case Print
