package parser.parsers

import cats.data.State
import lexer.Token

case class ParserState(tokens: List[Token], pos: Int):
  def current: Option[Token] = tokens.lift(pos)
  def advance: ParserState = copy(pos = pos + 1)
  def advanceN(n: Int): List[ParserState] = (0 to n).map(i => copy(pos = pos + i)).toList
