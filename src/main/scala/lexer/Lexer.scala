package lexer

class Lexer(private val source: String) {
  private var pos: Int = 0
  private var line: Int = 1
  private var column: Int = 1
  private var currentChar: Option[Char] = if (source.nonEmpty) Some(source.charAt(0)) else None

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

  private def consumeChar(): Unit =
    if (currentChar.contains('\n'))
      line += 1
      column = 1
    else column += 1

    pos += 1
    currentChar = if (pos < source.length) Some(source.charAt(pos)) else None

  private def consumeNChars(n: Int): Unit =
    (1 to n).foreach(_ => consumeChar())

  private def peekChar: Option[Char] = currentChar
  private def peekCharAhead(n: Int): Option[Char] =
    if (pos + n < source.length) Some(source.charAt(pos + n)) else None

  private def skipWhitespace(): Unit =
    while (peekChar.exists(c => c.isWhitespace))
      consumeChar()

  private def currentLocation: Location = Location(line, column)

  def nextToken(): Token =
    skipWhitespace()

    val location = currentLocation
    peekChar match
      case None =>
        Token(TokenType.EndOfFile, "", location)

      case Some(c) =>
        if (c.isLetter) lexIdentifierOrKeyword(location)
        else if (c.isDigit) lexNumber(location)
        else lexSymbolOrOperator(location)

  private def lexIdentifierOrKeyword(startLocation: Location): Token =
    val startPos = pos
    while (peekChar.exists(c => c.isLetterOrDigit || c == '_'))
      consumeChar()

    val lexeme = source.substring(startPos, pos)
    val tokenType = keywords.getOrElse(lexeme, TokenType.Identifier)

    Token(tokenType, lexeme, startLocation)

  private def lexNumber(startLocation: Location): Token =
    val startPos = pos
    var isReal = false

    while (peekChar.exists(c => c.isDigit)) {
      consumeChar()
    }

    // Check for decimal point
    if (peekChar.contains('.'))
      isReal = true
      consumeChar()
      while (peekChar.exists(c => c.isDigit))
        consumeChar()

    val lexeme = source.substring(startPos, pos)
    val tokenType = if (isReal) TokenType.RealLiteral else TokenType.IntegerLiteral

    Token(tokenType, lexeme, startLocation)

  private def lexSymbolOrOperator(startLocation: Location): Token =
    peekChar.get match
      case ':' =>
        consumeChar()
        if (peekChar.contains('='))
          consumeChar()
          Token(TokenType.Assignment, ":=", startLocation)
        else
          Token(TokenType.Colon, ":", startLocation)

      case ';' => consumeChar(); Token(TokenType.Semicolon, ";", startLocation)
      case ',' => consumeChar(); Token(TokenType.Comma, ",", startLocation)
      case '(' => consumeChar(); Token(TokenType.LeftParen, "(", startLocation)
      case ')' => consumeChar(); Token(TokenType.RightParen, ")", startLocation)
      case '[' => consumeChar(); Token(TokenType.LeftBracket, "[", startLocation)
      case ']' => consumeChar(); Token(TokenType.RightBracket, "]", startLocation)
      case '{' => consumeChar(); Token(TokenType.LeftBrace, "{", startLocation)
      case '}' => consumeChar(); Token(TokenType.RightBrace, "}", startLocation)

      case '.' =>
        consumeChar()
        if (peekChar.contains('.') && peekCharAhead(1).contains('.'))
          consumeNChars(2)
          Token(TokenType.RangeOp, "...", startLocation)
        else
          throw new LexerError(s"Unexpected character '.' at $startLocation")

      case '/' if peekCharAhead(1).contains('=') =>
        consumeNChars(2)
        Token(TokenType.Neq, "/=", startLocation)

      case '+' => consumeChar(); Token(TokenType.Plus, "+", startLocation)
      case '-' => consumeChar(); Token(TokenType.Minus, "-", startLocation)
      case '*' => consumeChar(); Token(TokenType.Mul, "*", startLocation)
      case '/' => Token(TokenType.Div, "/", startLocation)
      case '%' => consumeChar(); Token(TokenType.Mod, "%", startLocation)
      case '=' => consumeChar(); Token(TokenType.Eq, "=", startLocation)

      case '<' =>
        consumeChar()
        if (peekChar.contains('='))
          consumeChar(); Token(TokenType.Lteq, "<=", startLocation)
        else if (peekChar.contains('>'))
          consumeChar(); Token(TokenType.Neq, "<>", startLocation)
        else
          Token(TokenType.Lt, "<", startLocation)

      case '>' =>
        consumeChar()
        if (peekChar.contains('='))
          consumeChar(); Token(TokenType.Gteq, ">=", startLocation)
        else
          Token(TokenType.Gt, ">", startLocation)

      case c =>
        consumeChar()
        throw new LexerError(s"Unexpected character '$c' at $startLocation")
}

object Lexer:
  def tokenize(source: String): List[Token] =
    val lexer = new Lexer(source)
    Iterator.continually(lexer.nextToken())
      .takeWhile(_.tkType != TokenType.EndOfFile)
      .toList