package lexer

case class Position(
                     line: Int,
                     index: Int
                   )

case class Token(
                  tkType: TokenType,
                  value: String,
                  position: Position
                )