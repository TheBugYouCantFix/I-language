package lexer

enum TokenType:
  // Identifiers and Literals
  case Identifier
  case IntegerLiteral
  case RealLiteral
  case BooleanLiteral

  // Keywords
  case Var
  case Type
  case Routine
  case Return
  case If
  case Then
  case Else
  case For
  case While
  case Loop
  case Reverse
  case In
  case Print
  case End
  case Record
  case Array
  case Is

  // Type keywords
  case Integer
  case Real
  case Boolean

  // Boolean literals (also keywords)
  case True
  case False

  // Symbols & Operators
  case Dot
  case Colon
  case Semicolon
  case Comma
  case LeftParen
  case RightParen
  case LeftBracket
  case RightBracket
  case LeftBrace
  case RightBrace
  case Assignment
  case RangeOp

  // Arithmetic Operators
  case Plus
  case Minus
  case Mul
  case Div
  case Mod

  // Logical Operators
  case And
  case Or
  case Xor
  case Not

  // Relational Operators
  case Eq
  case Neq
  case Lt
  case Gt
  case Lteq
  case Gteq
  
  case EndOfFile
