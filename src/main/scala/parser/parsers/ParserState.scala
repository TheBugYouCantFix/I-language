package parser.parsers

import lexer.Token

case class ParserState(tokens: List[Token]):
  def peek: Option[Token] = tokens.headOption
  def advanceN(n: Int = 1): (List[Token], ParserState) = 
    tokens.splitAt(n) match
      case (retrieved, rest) => (retrieved, copy(rest))
  def discardN(n: Int = 1): ParserState = copy(tokens.drop(n))
