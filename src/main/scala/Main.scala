import lexer.*
import parser.*

object Main:
  def main(args: Array[String]): Unit =
//    lexerShowcase(
//      """
//        |var a : integer is 10
//        |var b is 20
//        |var flag : boolean is true
//        |var pi is 3.1415
//        |
//        |print a, b, flag, pi
//        """.stripMargin
//    )
//
    parserShowcase(
      """
        |type Person is record
        | var name : integer
        | var age : integer
        |end
        |
        |var p1 : Person
        |p1.name := 12
        |p1.age := 30
        |
        |print p1.name, p1.age
        |""".stripMargin
    )

//    parserShowcase(
//      """
//        |var a : integer is 10
//        |var b is 20
//        |var flag : boolean is true
//        |var pi is 3.1415
//        |
//        |print a, b, flag, pi
//        """.stripMargin
//    )
//
//    parserShowcase(
//      """
//        |type IntArray is array [5] integer
//        |
//        |var numbers : IntArray
//        |numbers[1] := 10
//        |numbers[2] := 20
//        |numbers[3] := 30
//        |numbers[4] := 40
//        |numbers[5] := 50
//        |
//        |for i in 1...5 loop
//        |    print numbers[i]
//        |end
//        |""".stripMargin
//    )
//
  private def parserShowcase(source: String): Unit =
    Lexer.tokenize(source) match
      case Left(er) => println(er)
      case Right(tokens) =>
        val program = SyntaxAnalyzer.analyze(tokens)
        println(parser.ASTPrinter.format(program))
