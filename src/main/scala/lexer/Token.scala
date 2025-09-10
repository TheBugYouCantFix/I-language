package lexer

case class Location(
                     line: Int,
                     index: Int
                   )

case class Token(
                  tkType: TokenType,
                  value: String,
                  position: Location
                )