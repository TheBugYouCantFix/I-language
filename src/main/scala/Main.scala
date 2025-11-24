import lexer.*
import parser.*
import semantic.*
import codegen.*

object Main:
  var c = 0
  def main(args: Array[String]): Unit =
    println("=".repeat(60))
    llvmShowcase(
      """
        |type Aboba is integer
        |var a : Aboba is 10
        |var b is 20
        |var sum is a + b
        |print sum
        |""".stripMargin
    )

    println("\n" + "=" * 60)
    llvmShowcase(
      """
        |type IntArray is array [3] integer
        |var numbers : IntArray
        |numbers[0] := 10
        |print numbers[0]
        |print numbers[1]
        |""".stripMargin
    )

    println("\n" + "=" * 60)
    llvmShowcase(
      """
        |routine add(x : integer, y : integer) : integer is
        |    x + y
        |end
        |
        |var result is add(5, 3)
        |print result
        |""".stripMargin
    )

    println("\n" + "=" * 60)
    llvmShowcase(
      """
        |routine multiply(x : integer, y : integer) : integer is
        |    var temp is x
        |    temp := temp * y
        |    temp
        |end
        |
        |var result is multiply(4, 3)
        |print result
        |""".stripMargin
    )

    println("\n" + "=" * 60)
    llvmShowcase(
      """
        |var flag : boolean is true
        |var count : integer is 0
        |
        |if flag then
        |    count := count + 1
        |end
        |print count
        |while count < 5 loop
        |    count := count + 1
        |    print count
        |end
        |""".stripMargin
    )

    println("\n" + "=" * 60)
    llvmShowcase(
        """
          |type Person is record
          | var name : integer
          | var age : integer
          |end
          |
          |var p1 : Person
          |p1.name := 12
          |p1.age := 30
          |print p1.name, p1.age
          |
          |p1.age := p1.age + 5
          |p1.name := p1.name + 7
          |print p1.name, p1.age
          |""".stripMargin
      )

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

  private def llvmShowcase(source: String): Unit =
    println() // Add spacing
    println(c)
    c = c + 1
    Lexer.tokenize(source) match
      case Left(er) => println(s"Lexer error: $er")
      case Right(tokens) =>
        println("=== Parsing ===")
        val program = SyntaxAnalyzer.analyze(tokens)
        println("\nAST:")
        println(parser.ASTPrinter.format(program))
        println("Program parsed successfully")

        println("\n=== Semantic Analysis ===")
        val semResult = SemanticAnalyzer.analyze(program)
        if semResult.errors.nonEmpty then
          println(s"Found ${semResult.errors.length} error(s):")
          semResult.errors.zipWithIndex.foreach { case (error, idx) =>
            println(s"  [${idx + 1}] ${error.getMessage}")
          }
          println("\nCannot generate LLVM code due to semantic errors.")
        else
          println("No semantic errors found!")

          println("\n=== LLVM IR Code Generation ===")
          val llvmIR = LLVMCodeGenerator.generate(program)
          println(llvmIR)

          println("\n=== Save to file ===")
          val outputFile = java.io.File("output.ll")
          java.nio.file.Files.write(java.nio.file.Paths.get("output.ll"), llvmIR.getBytes)
          println(s"LLVM IR written to: ${outputFile.getAbsolutePath}")
          println(s"to execute th llvm IR run: clang output.ll -o program && ./program")
