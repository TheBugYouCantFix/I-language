import lexer.*
import parser.*
import semantic.*

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
//    parserShowcase(
//      """
//        |var n is 2 + 2 * 2
//        |""".stripMargin
//    )
    
//    println("=".repeat(60))
//    println("Example 1: Successful Analysis - Type inference and declarations")
//    println("=".repeat(60))
//    semanticShowcase(
//      """
//        |var a : integer is 10
//        |var b is 20
//        |var sum is a + b
//        |""".stripMargin
//    )

//    println("\n" + "=" * 60)
//    println("Example 2: Type Mismatch Error")
//    println("=".repeat(60))
//    semanticShowcase(
//      """
//        |var a : integer is 10
//        |var b : boolean is true
//        |var c is a + b
//        |""".stripMargin
//    )

//    println("\n" + "=" * 60)
//    println("Example 3: Undeclared Variable Error")
//    println("=".repeat(60))
//    semanticShowcase(
//      """
//        |var a is 10
//        |var b is unknownVar + 5
//        |""".stripMargin
//    )
////
//    println("\n" + "=" * 60)
//    println("Example 4: Array Type and Bounds")
//    println("=".repeat(60))
//    semanticShowcase(
//      """
//        |type IntArray is array [3] integer
//        |var numbers : IntArray
//        |numbers[0] := 10
//        |numbers[4] := 20
//        |""".stripMargin
//    )

//    println("\n" + "=" * 60)
//    println("Example 5: Routine Declaration and Call")
//    println("=".repeat(60))
//    semanticShowcase(
//      """
//        |routine add(x : integer, y : integer) : integer is
//        |    x + y
//        |end
//        |
//        |var result is add(5, 3)
//        |""".stripMargin
//    )
//
//    println("\n" + "=" * 60)
//    println("Example 6: Routine Call Errors")
//    println("=".repeat(60))
//    semanticShowcase(
//      """
//        |routine add(x : integer, y : integer) : integer is
//        |    x + y
//        |end
//        |
//        |var result1 is add(5)
//        |var result2 is add(5, 3, 2)
//        |var result3 is add(5, true)
//        |""".stripMargin
//    )
//
//    println("\n" + "=" * 60)
//    println("Example 7: Undeclared Routine")
//    println("=".repeat(60))
//    semanticShowcase(
//      """
//        |var result is unknownFunction(5)
//        |""".stripMargin
//    )
//
    println("\n" + "=" * 60)
    println("Example 8: Control Flow - If/While with Boolean Conditions")
    println("=".repeat(60))
    semanticShowcase(
      """
        |var flag : boolean is true
        |var count : integer is 0
        |
        |if flag then
        |    count := count + 1
        |end
        |
        |while count < 5 loop
        |    count := count + 1
        |end
        |""".stripMargin
    )

    println("\n" + "=" * 60)
    println("Example 9: Control Flow Type Errors")
    println("=".repeat(60))
    semanticShowcase(
      """
        |var num : integer is 5
        |
        |if num then
        |    num := 10
        |end
        |
        |while num loop
        |    num := num - 1
        |end
        |""".stripMargin
    )

    println("\n" + "=" * 60)
    println("Example 10: Duplicate Declarations")
    println("=".repeat(60))
    semanticShowcase(
      """
        |var x : integer is 10
        |var x : integer is 20
        |var y is 5
        |var y is 6
        |""".stripMargin
    )

    semanticShowcase(
      """
        |var a : integer is 10
        |var b : boolean is true
        |var c is a + b
        |""".stripMargin
    )

    semanticShowcase(
      """
        |type IntArray is array [5] integer
        |var numbers : IntArray
        |numbers[1] := 10
        |numbers[10] := 20
        |""".stripMargin
    )

    semanticShowcase(
      """
        |routine add(x : integer, y : integer) : integer is
        |    x + y
        |end
        |
        |var result is add(5, 3)
        |""".stripMargin
    )


    semanticShowcase(
      """
        |var a is 1
        |print a
        |""".stripMargin
    )
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
//        |for i in 1 .. 5 loop
//        |    print numbers[i]
//        |end
//        |""".stripMargin
//    )

  private def parserShowcase(source: String): Unit =
    Lexer.tokenize(source) match
      case Left(er) => println(er)
      case Right(tokens) =>
        println(tokens)
        val program = SyntaxAnalyzer.analyze(tokens)
        println(parser.ASTPrinter.format(program))

  private def semanticShowcase(source: String): Unit =
    println() // Add spacing between examples
    Lexer.tokenize(source) match
      case Left(er) => println(s"Lexer error: $er")
      case Right(tokens) =>
        println("=== Tokens ===")
        println(tokens)
        println("\n=== Parsed AST ===")
        val program = SyntaxAnalyzer.analyze(tokens)
        println(parser.ASTPrinter.format(program))
        println(s"\nDeclarations count: ${program.declarations.length}")
        program.declarations.foreach {
          case d: parser.structures.StatementDeclaration => 
            println(s"  StatementDeclaration with ${d.statements.length} statements")
          case d => 
            println(s"  ${d.getClass.getSimpleName}")
        }
        println("\n=== Semantic Analysis ===")
        val result = SemanticAnalyzer.analyze(program)
        if result.errors.nonEmpty then
          println(s"Found ${result.errors.length} error(s):")
          result.errors.zipWithIndex.foreach { case (error, idx) =>
            println(s"  [${idx + 1}] ${error.getMessage}")
          }
        else
          println("No semantic errors found!")
        println("\n=== Optimized Program ===")
        result.optimizedProgram match
          case Some(optimized) =>
            if optimized.declarations.isEmpty then
              println("(Empty program - all declarations were removed as unused)")
              println("Note: This happens when variables are declared but never referenced.")
            else
              println(parser.ASTPrinter.format(optimized))
              println(s"Optimized declarations: ${optimized.declarations.length}")
          case None =>
            println("(No optimized program available)")
