import lexer.LexerF

object Main:
  def main(args: Array[String]): Unit =
    val res = LexerF.tokenize(
      """
        var a : integer is 10
        var b is 20
        var flag : boolean is true
        var pi is 3.1415

        print a, b, flag, pi
        """.stripMargin
    )

    println(res)
