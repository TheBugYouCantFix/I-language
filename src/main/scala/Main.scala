import lexer.{Lexer, Token}

object Main:
  def main(args: Array[String]): Unit =
    lexerShowcase(
      """
        var a : integer is 10
        var b is 20
        var flag : boolean is true
        var pi is 3.1415

        print a, b, flag, pi
        """.stripMargin
    )

  private def lexerShowcase(source: String) =
    Lexer.tokenize(source).foreach {
      case Token(tkType, value, location) =>
        println("Token:")
        println(s" - type: $tkType")
        println(s" - value: $value")
        println(s" - position: $location")
    }
