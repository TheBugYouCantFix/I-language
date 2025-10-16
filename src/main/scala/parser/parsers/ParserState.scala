package parser.parsers

import cats.data.State
import lexer.Token

case class ParserState(tokens: List[Token]): // TODO: bring state monad
  def peek: Option[Token] = tokens.headOption
  def advanceN(n: Int = 1): (List[Token], ParserState) = 
    tokens.splitAt(n) match
      case (retrieved, rest) => (retrieved, copy(rest))
  def discardN(n: Int = 1): ParserState = copy(tokens.drop(n))

// TODO: implement proper ADT (i. e. empty state)
