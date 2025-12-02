import lexer.*
import parser.*
import semantic.*
import codegen.*
import compiler.CompilerError

object Main:
  var c = 0
  def main(args: Array[String]): Unit =

//    llvmShowcase(
//      """
//        |var a is 2 + 3
//        |print a
//        |""".stripMargin
//    )
//
//    llvmShowcase(
//     """|routine fac(x: integer): integer is
//        | if n = 0 or n = 1
//        | then n
//        | else n * fac(n - 1)
//        | end
//        |end
//        |
//        |var res is fac(5)
//        |print res
//        |""".stripMargin)
//     llvmShowcase(
//        """
//          |routine isEven(x: integer): boolean is
//          | print x
//          | x % 2 = 0
//          |end
//          |
//          |var res is isEven(2)
//          |print res
//          |""".stripMargin)
//    llvmShowcase(
//      """
//        |routine add(x: integer, y: integer): integer is
//        | x + y
//        |end
//        |
//        |var res is add(3, 2)
//        |print res
//        |""".stripMargin)

    // record modification func
//    llvmShowcase(
//      """
//        |type Person is record
//        | var name : integer
//        | var age : integer
//        |end
//        |routine foo(r: Person): Person is
//        |  r.age := 10
//        |  r
//        |end
//        |var p1 : Person
//        |var res is foo(p1)
//        |print res.age
//        |""".stripMargin
//    )

    // nested records
//    llvmShowcase(
//      """
//        |type Person is record
//        | var name : integer
//        | var age : integer
//        |end
//        |type A is record
//        | var p : Person
//        |end
//        |routine foo(a: A): A is
//        |  a.p.age := 2
//        |  a
//        |end
//        |var p1 : Person
//        |p1.age := 1
//        |var a : A
//        |a.p := p1
//        |print p1.age
//        |var res is foo(a)
//        |print res.p.age
//        |""".stripMargin
//    )

    // array of records
    llvmShowcase(
      """
        |type Person is record
        | var age : integer
        | var weight : integer
        |end
        |
        |var n is 4
        |type PersonArr is array[5] Person
        |
        |var arr: PersonArr
        |for i in 0 .. n loop
        | var p : Person
        | p.age := i * 2
        | if i = n
        | then p.weight := i + 10
        | else p.weight := i + 3
        | end
        | arr[i] := p
        |end
        |
        |for i in 0 .. n loop
        | var res is arr[i]
        | print res.age, res.weight
        |end
        |""".stripMargin)

    // factorial
//    llvmShowcase(
//      """
//        |routine fac(n: integer): integer is
//        | var acc is 1
//        | for i in 2 .. n loop
//        |   acc := acc * i
//        | end
//        | acc
//        |end
//        |
//        |var res is fac(6)
//        |print res
//        |""".stripMargin)

  private def llvmShowcase(source: String): Unit =
    println() // Add spacing
    println(c)
    c = c + 1
    Lexer.tokenize(source) match
      case Left(er) => println(s"Lexer error: $er}")
      case Right(tokens) =>
        println("tokens:")
        tokens.foreach(println)
        println("=== Parsing ===")
        SyntaxAnalyzer.analyze(tokens) match
          case Left(error) =>
            println(s"Parser error: ${error.message}")
            error.cause.foreach { cause =>
              println(s"  Caused by: ${cause.getMessage}")
              cause.printStackTrace()
            }
          case Right(program) =>
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
              println(s"Optimized AST: ${parser.ASTPrinter.format(semResult.optimizedProgram.getOrElse(program))}")

              println("\n=== LLVM IR Code Generation ===")
              LLVMCodeGenerator.generate(semResult.optimizedProgram.getOrElse(program)) match
                case Left(error) =>
                  println(s"Code generation error: ${error.message}")
                  error.cause.foreach { cause =>
                    println(s"  Caused by: ${cause.getMessage}")
                    cause.printStackTrace()
                  }
                case Right(llvmIR) =>
                  println(llvmIR)

                  println("\n=== Save to file ===")
                  val outputFile = java.io.File("output.ll")
                  java.nio.file.Files.write(java.nio.file.Paths.get("output.ll"), llvmIR.getBytes)
                  println(s"LLVM IR written to: ${outputFile.getAbsolutePath}")
                  println(s"to execute th llvm IR run: clang output.ll -o program && ./program")
