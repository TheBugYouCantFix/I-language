package lexer

case class Location(
                   line: Int,
                   column: Int
                   )

case class Token(
                tkType: TokenType,
                value: String,
                location: Location
                )