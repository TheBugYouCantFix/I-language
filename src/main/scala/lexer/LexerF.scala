package lexer

import scala.annotation.tailrec

case class LexerState(
                       source: String,
                       pos: Int = 0,
                       line: Int = 1,
                       column: Int = 1
                     ):
  def currentChar: Option[Char] =
    if pos < source.length then Some(source.charAt(pos)) else None

  def peekAhead(n: Int): Option[Char] =
    if pos + n < source.length then Some(source.charAt(pos + n)) else None

  def advance: LexerState = currentChar match
    case Some('\n') => copy(pos = pos + 1, line = line + 1, column = 1)
    case Some(_)    => copy(pos = pos + 1, column = column + 1)
    case None       => this

  def advanceN(n: Int): LexerState =
    (1 to n).foldLeft(this)((state, _) => state.advance)

  def currentLocation: Position = Position(line, column)

  def substring(start: Int): String = source.substring(start, pos)

object LexerF:
  private val keywords: Map[String, TokenType] = Map(
    "var" -> TokenType.Var,
    "type" -> TokenType.Type,
    "routine" -> TokenType.Routine,
    "return" -> TokenType.Return,
    "if" -> TokenType.If,
    "then" -> TokenType.Then,
    "else" -> TokenType.Else,
    "for" -> TokenType.For,
    "while" -> TokenType.While,
    "loop" -> TokenType.Loop,
    "reverse" -> TokenType.Reverse,
    "in" -> TokenType.In,
    "print" -> TokenType.Print,
    "end" -> TokenType.End,
    "record" -> TokenType.Record,
    "array" -> TokenType.Array,
    "is" -> TokenType.Is,
    "integer" -> TokenType.Integer,
    "real" -> TokenType.Real,
    "boolean" -> TokenType.Boolean,
    "true" -> TokenType.True,
    "false" -> TokenType.False
  )

  def tokenize(source: String): Either[LexerError, List[Token]] =
    @tailrec
    def loop(state: LexerState, acc: List[Token]): Either[LexerError, List[Token]] =
      val newState = skipWhitespace(state)
      newState.currentChar match
        case None => Right(acc.reverse)
        case Some(_) =>
          nextToken(newState) match
            case Right((nextState, token)) => loop(nextState, token :: acc)
            case Left(error) => Left(error)

    loop(LexerState(source), Nil)

  @tailrec
  def skipWhitespace(current: LexerState): LexerState =
    current.currentChar match
      case Some(c) if c.isWhitespace => skipWhitespace(current.advance)
      case _ => current

  private def nextToken(state: LexerState): Either[LexerError, (LexerState, Token)] =
    val location = state.currentLocation
    state.currentChar match
      case None => Right((state, Token(TokenType.EndOfFile, "", location)))
      case Some(c) if c.isLetter => lexIdentifierOrKeyword(state, location)
      case Some(c) if c.isDigit => lexNumber(state, location)
      case Some(_) => lexSymbolOrOperator(state, location)

  private def lexIdentifierOrKeyword(state: LexerState, startLocation: Position): Either[LexerError, (LexerState, Token)] =
    @tailrec
    def consumeLetters(current: LexerState): LexerState =
      current.currentChar match
        case Some(c) if c.isLetterOrDigit || c == '_' => consumeLetters(current.advance)
        case _ => current

    val startPos = state.pos
    val endState = consumeLetters(state)
    val lexeme = endState.substring(startPos)
    val tokenType = keywords.getOrElse(lexeme, TokenType.Identifier)

    Right((endState, Token(tokenType, lexeme, startLocation)))

  private def lexNumber(state: LexerState, startLocation: Position): Either[LexerError, (LexerState, Token)] =
    @tailrec
    def consumeDigits(current: LexerState): LexerState =
      current.currentChar match
        case Some(c) if c.isDigit => consumeDigits(current.advance)
        case _ => current

    val startPos = state.pos
    val afterDigits = consumeDigits(state)

    val (endState, isReal) = afterDigits.currentChar match
      case Some('.') =>
        val afterDot = afterDigits.advance
        val afterFraction = consumeDigits(afterDot)
        (afterFraction, true)
      case _ => (afterDigits, false)

    val lexeme = endState.substring(startPos)
    val tokenType = if isReal then TokenType.RealLiteral else TokenType.IntegerLiteral

    Right((endState, Token(tokenType, lexeme, startLocation)))

  private def lexSymbolOrOperator(state: LexerState, startLocation: Position): Either[LexerError, (LexerState, Token)] =
    state.currentChar match
      case Some(':') =>
        val afterColon = state.advance
        afterColon.currentChar match
          case Some('=') =>
            Right((afterColon.advance, Token(TokenType.Assignment, ":=", startLocation)))
          case _ =>
            Right((afterColon, Token(TokenType.Colon, ":", startLocation)))

      case Some(';') => Right((state.advance, Token(TokenType.Semicolon, ";", startLocation)))
      case Some(',') => Right((state.advance, Token(TokenType.Comma, ",", startLocation)))
      case Some('(') => Right((state.advance, Token(TokenType.LeftParen, "(", startLocation)))
      case Some(')') => Right((state.advance, Token(TokenType.RightParen, ")", startLocation)))
      case Some('[') => Right((state.advance, Token(TokenType.LeftBracket, "[", startLocation)))
      case Some(']') => Right((state.advance, Token(TokenType.RightBracket, "]", startLocation)))
      case Some('{') => Right((state.advance, Token(TokenType.LeftBrace, "{", startLocation)))
      case Some('}') => Right((state.advance, Token(TokenType.RightBrace, "}", startLocation)))

      case Some('.') =>
        val afterFirstDot = state.advance
        (afterFirstDot.currentChar, afterFirstDot.peekAhead(1)) match
          case (Some('.'), Some('.')) =>
            Right((afterFirstDot.advanceN(2), Token(TokenType.RangeOp, "...", startLocation)))
          case _ => Right((state.advance, Token(TokenType.Dot, ".", startLocation)))

      case Some('/') =>
        state.peekAhead(1) match
          case Some('=') =>
            Right((state.advanceN(2), Token(TokenType.Neq, "/=", startLocation)))
          case _ =>
            Right((state.advance, Token(TokenType.Div, "/", startLocation)))

      case Some('+') => Right((state.advance, Token(TokenType.Plus, "+", startLocation)))
      case Some('-') => Right((state.advance, Token(TokenType.Minus, "-", startLocation)))
      case Some('*') => Right((state.advance, Token(TokenType.Mul, "*", startLocation)))
      case Some('%') => Right((state.advance, Token(TokenType.Mod, "%", startLocation)))
      case Some('=') => Right((state.advance, Token(TokenType.Eq, "=", startLocation)))

      case Some('<') =>
        val afterLt = state.advance
        afterLt.currentChar match
          case Some('=') => Right((afterLt.advance, Token(TokenType.Lteq, "<=", startLocation)))
          case Some('>') => Right((afterLt.advance, Token(TokenType.Neq, "<>", startLocation)))
          case _ => Right((afterLt, Token(TokenType.Lt, "<", startLocation)))

      case Some('>') =>
        val afterGt = state.advance
        afterGt.currentChar match
          case Some('=') => Right((afterGt.advance, Token(TokenType.Gteq, ">=", startLocation)))
          case _ => Right((afterGt, Token(TokenType.Gt, ">", startLocation)))

      case Some(c) =>
        Left(LexerError(s"Unexpected character '$c' at $startLocation"))

      case None =>
        Right((state, Token(TokenType.EndOfFile, "", startLocation)))