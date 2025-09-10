import lexer.*

object Main:
  def main(args: Array[String]): Unit =
    lexerShowcase(
      """
        |var a : integer is 10
        |var b is 20
        |var flag : boolean is true
        |var pi is 3.1415
        |
        |print a, b, flag, pi
        """.stripMargin
    )

    println()

//    lexerShowcase(
//      """
//        |type Person is record
//        | var name : string
//        | var age : integer
//        |end
//        |
//        |var p1 : Person
//        |p1.name := "Alice"
//        |p1.age := 30
//        |
//        |print p1.name, p1.age
//        |""".stripMargin
//    )

  private def lexerShowcase(source: String) =
    LexerF.tokenize(source) match
      case Left(er) => println(er)
      case Right(res) =>
        res.foreach {
          case Token(tkType, value, Position(ln, ind)) =>
            println("Token:")
            println(s" - type: $tkType")
            println(s" - value: $value")
            println(s" - position: line $ln, index $ind")
        }
