package codegen

import parser.structures.*
import cats.data.State
import scala.annotation.tailrec

object LLVMCodeGenerator {
  
  case class CodeGenState(
    counter: Int = 1,  // Start from 1 for sequential numbering %1, %2, %3, etc.
    labelCounter: Int = 0,
    variables: Map[String, String] = Map.empty, // name -> register (pointer)
    variableTypes: Map[String, String] = Map.empty, // name -> LLVM type (e.g., "i32", "i1")
    variableAllocaTypes: Map[String, String] = Map.empty, // name -> LLVM alloca type (e.g., "[5 x i32]", "i32")
    variableTypeDefs: Map[String, Type] = Map.empty,
    functions: Map[String, RoutineHeader] = Map.empty,
    types: Map[String, Type] = Map.empty,
    currentFunction: Option[String] = None
  ) {
    def nextRegister(): (CodeGenState, String) = 
      val reg = s"%${counter}"
      (copy(counter = counter + 1), reg)
    
    def nextLabel(): (CodeGenState, String) =
      val label = s"label_${labelCounter}"
      (copy(labelCounter = labelCounter + 1), label)
    
    def addVariable(name: String, reg: String): CodeGenState =
      copy(variables = variables.updated(name, reg))
    
    def addVariableWithType(name: String, reg: String, llvmType: String): CodeGenState =
      copy(variables = variables.updated(name, reg), variableTypes = variableTypes.updated(name, llvmType))
    
    def addVariableWithAllocaType(name: String, reg: String, llvmType: String, allocaType: String, originalType: Option[Type] = None): CodeGenState =
      copy(
        variables = variables.updated(name, reg),
        variableTypes = variableTypes.updated(name, llvmType),
        variableAllocaTypes = variableAllocaTypes.updated(name, allocaType),
        variableTypeDefs =
          originalType match
            case Some(t) => variableTypeDefs.updated(name, t)
            case None    => variableTypeDefs
      )
    
    def addFunction(name: String, header: RoutineHeader): CodeGenState =
      copy(functions = functions.updated(name, header))
    
    def addType(name: String, t: Type): CodeGenState =
      copy(types = types.updated(name, t))
    
    def setCurrentFunction(fn: Option[String]): CodeGenState =
      copy(currentFunction = fn)
  }
  
  type CodeGen[A] = State[CodeGenState, A]

  def generate(program: Program): String = {
    val initialState = CodeGenState()
    
    // First pass: collect types and function headers
    val state1 = program.declarations.foldLeft(initialState) { (st, decl) =>
      decl match
        case TypeDeclaration(name, typeDef) => st.addType(name, typeDef)
        case RoutineDeclaration(header, _) => st.addFunction(header.identifier, header)
        case _ => st
    }
    
    // Second pass: generate function definitions
    val (state2, functionCode) = program.declarations.foldLeft((state1, "")) { case ((st, acc), decl) =>
      decl match
        case RoutineDeclaration(header, Some(body)) =>
          val (newSt, code) = generateFunction(header, body, st)
          (newSt, acc + code)
        case _ => (st, acc)
    }
    
    // Third pass: generate main function
    val (state3, mainCode) = generateMain(program, state2)
    
    // Build final module - format strings must come before functions that use them
    val stdLib = "\n; Standard library declarations\n" +
      "declare i32 @printf(i8*, ...)\n" +
      "declare i32 @putchar(i32)\n"
    
    val formatStrings = "\n; Format strings\n" +
      "@.str.int = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\"\n" +
      "@.str.double = private unnamed_addr constant [5 x i8] c\"%lf\\0A\\00\"\n" +
      "@.str.bool = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\"\n"
    
    "; LLVM IR generated from ILang\n" +
      "target datalayout = \"e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128\"\n" +
      "target triple = \"x86_64-pc-linux-gnu\"\n\n" +
      formatStrings +
      stdLib +
      functionCode +
      mainCode
  }
  
  private def generateMain(program: Program, state: CodeGenState): (CodeGenState, String) = {
    val sb = new StringBuilder
    sb.append("\n; Main function\n")
    sb.append("define i32 @main() {\n")
    sb.append("entry:\n")
    
    val (finalState, code) = program.declarations.foldLeft((state, "")) { case ((st, acc), decl) =>
      decl match
        case VariableDeclaration(name, typeOpt, initOpt) =>
          val declaredType = typeOpt.getOrElse(IntegerType)
          val allocaType = typeToLLVMAllocaType(declaredType, st)
          val valueType = typeToLLVMType(declaredType, st)
          // First allocate the register for alloca
          val (st1, reg) = st.nextRegister()
          val st2 = st1.addVariableWithAllocaType(name, reg, valueType, allocaType, Some(declaredType))
          // Generate alloca - alloca returns a pointer, so we use the alloca type
          val allocaCode = s"  $reg = alloca $allocaType\n"
          initOpt match
            case Some(expr) =>
              // Then generate the expression code
              val (st3, exprCode, valueReg, exprType) = generateExpression(expr, st2)
              val storeCode = s"  store $valueType $valueReg, $allocaType* $reg\n"
              (st3, acc + allocaCode + exprCode + storeCode)
            case None =>
              val zeroValue = zeroValueForType(allocaType)
              val storeCode = s"  store $allocaType $zeroValue, $allocaType* $reg\n"
              (st2, acc + allocaCode + storeCode)
        
        case StatementDeclaration(statements) =>
          val (st1, code) = statements.foldLeft((st, "")) { case ((s, c), stmt) =>
            val (s1, stmtCode) = generateStatement(stmt, s)
            (s1, c + stmtCode)
          }
          (st1, acc + code)
        
        case _ => (st, acc)
    }
    
    sb.append(code)
    sb.append("  ret i32 0\n")
    sb.append("}\n")
    (finalState, sb.toString())
  }

  private def generateFunctionDecl(header: RoutineHeader, state: CodeGenState): String = {
    val retType = header.returnType.map(typeToLLVMType(_, state)).getOrElse("void")
    val params = header.parameters.map(p => s"${typeToLLVMType(p.parameterType, state)} %${p.identifier}").mkString(", ")
    s"declare $retType @${header.identifier}($params)\n"
  }

  private def generateFunction(header: RoutineHeader, body: RoutineBody, state: CodeGenState): (CodeGenState, String) = {
    val state1 = state.setCurrentFunction(Some(header.identifier))
    val retType = header.returnType.map(typeToLLVMType(_, state1)).getOrElse("void")
    val params = header.parameters.map(p => s"${typeToLLVMType(p.parameterType, state1)} %${p.identifier}").mkString(", ")
    
    var sb = s"\n; Function: ${header.identifier}\n" +
      s"define $retType @${header.identifier}($params) {\n" +
      "entry:\n"
    
    // Allocate parameters - alloca registers first, then load registers
    val (state2, paramCode) = header.parameters.foldLeft((state1, "")) { case ((st, acc), p) =>
      val paramType = typeToLLVMType(p.parameterType, st)
      // Allocate alloca register first
      val (st1, allocaReg) = st.nextRegister()
      // Store parameter to alloca
      val code = acc + s"  $allocaReg = alloca $paramType\n" +
        s"  store $paramType %${p.identifier}, $paramType* $allocaReg\n"
      // Store the alloca register (pointer) in variables map with its type
          val st2 = st1.addVariableWithAllocaType(p.identifier, allocaReg, paramType, paramType, Some(p.parameterType))
      (st2, code)
    }
    sb += paramCode
    
    val (state3, bodyCode, hasReturn) = body match
      case JustRoutineBody(b) =>
        val (st1, declCode) = b.declarations.foldLeft((state2, "")) { case ((st, acc), decl) =>
          decl match
            case VariableDeclaration(name, typeOpt, initOpt) =>
              val allocaType = typeToLLVMAllocaType(typeOpt.getOrElse(IntegerType), st)
              val valueType = typeToLLVMType(typeOpt.getOrElse(IntegerType), st)
              // First allocate the register for alloca
              val (st1, reg) = st.nextRegister()
              val st2 = st1.addVariableWithType(name, reg, valueType)
              // Generate alloca - alloca returns a pointer, so we use the alloca type
              val allocaCode = s"  $reg = alloca $allocaType\n"
              val (st3, initCode, valueReg) = initOpt match
                case Some(expr) =>
                  // Then generate the expression code
                  val (st3, code, reg, regType) = generateExpression(expr, st2)
                  (st3, code, reg)
                case None => (st2, "", "0")
              val zeroValue = allocaType match
                case t if t.endsWith("*") => "null"
                case "i1" => "0"
                case "i32" => "0"
                case "double" => "0.0"
                case _ => "null"
              val code = acc + allocaCode + initCode +
                (if initOpt.isDefined then s"  store $valueType $valueReg, $allocaType* $reg\n" else s"  store $allocaType $zeroValue, $allocaType* $reg\n")
              (st3, code)
            case _ => (st, acc)
        }
        // Check if the last statement is a return expression (PrintStatement with single expr)
        val (statementsToProcess, hasReturnExpr) = b.statements.lastOption match
          case Some(PrintStatement(List(expr))) =>
            // This is a return expression, not a print
            (b.statements.init, Some(expr))
          case _ => (b.statements, None)
        
        val (st2, stmtCode) = statementsToProcess.foldLeft((st1, "")) { case ((st, acc), stmt) =>
          val (st1, code) = generateStatement(stmt, st)
          (st1, acc + code)
        }
        
        val (st3, finalCode, hasRet) = hasReturnExpr match
          case Some(expr) =>
            // Generate return from expression
            val (stExpr, exprCode, exprReg, exprType) = generateExpression(expr, st2)
            val retType = header.returnType.map(typeToLLVMType(_, stExpr)).getOrElse("i32")
            val retCode = s"  ret $retType $exprReg\n"
            (stExpr, stmtCode + exprCode + retCode, true)
          case None =>
            (st2, stmtCode, stmtCode.contains("ret "))
        
        (st3, declCode + finalCode, hasRet)
      
      case RoutineBodyExpression(expr) =>
        val (st1, code, valueReg, valueType) = generateExpression(expr, state2)
        val retCode = header.returnType match
          case Some(t) => s"  ret ${typeToLLVMType(t, st1)} $valueReg\n"
          case None => "  ret void\n"
        (st1, code + retCode, true)
    
    sb += bodyCode
    if !hasReturn then
      sb += (if retType == "void" then "  ret void\n" else s"  ret $retType 0\n")
    
    sb += "}\n"
    (state3.setCurrentFunction(state.currentFunction), sb)
  }

  private def generateStatement(stmt: Statement, state: CodeGenState): (CodeGenState, String) = stmt match
    case Assignment(target, value) =>
      val (st1, code, valueReg, valueType) = generateExpression(value, state)
      val (st2, targetCode, targetReg) = generateModifiablePrimary(target, st1)
      // Check if target has array accesses - if so, targetReg is already a pointer to the element
      val result = code + targetCode + s"  store $valueType $valueReg, $valueType* $targetReg\n"
      (st2, result)
    
    case RoutineCall(id, args) =>
      val (st1, argCode, argRegs) = args.foldLeft((state, "", List.empty[String])) { case ((st, acc, regs), arg) =>
        val (st1, code, reg, regType) = generateExpression(arg, st)
        (st1, acc + code, regs :+ reg)
      }
      val func = st1.functions.get(id)
      val retType = func.flatMap(_.returnType).map(typeToLLVMType(_, st1)).getOrElse("void")
      val paramTypes = func.map(_.parameters.map(_.parameterType).map(typeToLLVMType(_, st1))).getOrElse(Nil)
      val (st2, callReg) = if retType == "void" then (st1, "") else st1.nextRegister()
      val argsStr = argRegs.zip(paramTypes).map { case (r, t) => s"$t $r" }.mkString(", ")
      val result = argCode + (if retType == "void" then
        s"  call $retType @$id($argsStr)\n"
      else
        s"  $callReg = call $retType @$id($argsStr)\n")
      (st2, result)
    
    case WhileLoop(condition, body) =>
      val (st1, startLabel) = state.nextLabel()
      val (st2, bodyLabel) = st1.nextLabel()
      val (st3, endLabel) = st2.nextLabel()
      val (st4, condCode, condReg, condType) = generateExpression(condition, st3)
      val (st5, bodyCode) = generateBody(body, st4)
      val result = s"  br label %$startLabel\n" +
        s"$startLabel:\n" +
        condCode +
        s"  br i1 $condReg, label %$bodyLabel, label %$endLabel\n" +
        s"$bodyLabel:\n" +
        bodyCode +
        s"  br label %$startLabel\n" +
        s"$endLabel:\n"
      (st5, result)
    
    case ForLoop(loopVar, range, isReverse, body) =>
      val (st1, startCode, startReg, startType) = generateExpression(range.start, state)
      val (st2, endCode, endReg, endType) = range.end match
        case Some(e) => generateExpression(e, st1)
        case None => (st1, "", "0", "i32")
      val (st3, loopVarReg) = st2.nextLabel()
      val (st4, startLabel) = st3.nextLabel()
      val (st5, bodyLabel) = st4.nextLabel()
      val (st6, incLabel) = st5.nextLabel()
      val (st7, endLabel) = st6.nextLabel()
      val (st8, currentReg) = st7.nextRegister()
      val (st9, cmpReg) = st8.nextRegister()
      val st10 = st9.addVariable(loopVar, currentReg)
      val (st11, bodyCode) = generateBody(body, st10)
      val (st12, nextReg) = st11.nextRegister()
      val result = startCode +
        endCode +
        s"  %$loopVarReg = alloca i32\n" +
        s"  store i32 $startReg, i32* %$loopVarReg\n" +
        s"  br label %$startLabel\n" +
        s"$startLabel:\n" +
        s"  $currentReg = load i32, i32* %$loopVarReg\n" +
        (if isReverse then
          s"  $cmpReg = icmp sge i32 $currentReg, $endReg\n"
        else
          s"  $cmpReg = icmp sle i32 $currentReg, $endReg\n") +
        s"  br i1 $cmpReg, label %$bodyLabel, label %$endLabel\n" +
        s"$bodyLabel:\n" +
        bodyCode +
        s"  br label %$incLabel\n" +
        s"$incLabel:\n" +
        (if isReverse then
          s"  $nextReg = sub i32 $currentReg, 1\n"
        else
          s"  $nextReg = add i32 $currentReg, 1\n") +
        s"  store i32 $nextReg, i32* %$loopVarReg\n" +
        s"  br label %$startLabel\n" +
        s"$endLabel:\n"
      (st12, result)
    
    case IfStatement(condition, thenBody, elseBody) =>
      val (st1, condCode, condReg, condType) = generateExpression(condition, state)
      // Convert condition to boolean if needed
      val (st2, boolCode, boolReg) = ensureBoolean(condReg, condType, st1)
      val (st3, thenLabel) = st2.nextLabel()
      val (st4, elseLabel) = st3.nextLabel()
      val (st5, endLabel) = st4.nextLabel()
      val (st6, thenCode) = generateBody(thenBody, st5)
      val (st7, elseCode) = elseBody match
        case Some(eb) =>
          val (st, code) = generateBody(eb, st6)
          (st, code)
        case None => (st6, "")
      val result = condCode + boolCode +
        (elseBody match
          case Some(_) =>
            s"  br i1 $boolReg, label %$thenLabel, label %$elseLabel\n" +
            s"$thenLabel:\n" +
            thenCode +
            s"  br label %$endLabel\n" +
            s"$elseLabel:\n" +
            elseCode +
            s"  br label %$endLabel\n"
          case None =>
            s"  br i1 $boolReg, label %$thenLabel, label %$endLabel\n" +
            s"$thenLabel:\n" +
            thenCode +
            s"  br label %$endLabel\n") +
        s"$endLabel:\n"
      (st7, result)
    
    case PrintStatement(values) =>
      val (finalState, code) = values.foldLeft((state, "")) { case ((st, acc), expr) =>
        val (st1, exprCode, reg, regType) = generateExpression(expr, st)
        val (st2, formatPtr) = st1.nextRegister()
        val (st3, callReg) = st2.nextRegister()
        val printCode = exprCode +
          s"  $formatPtr = getelementptr inbounds [4 x i8], [4 x i8]* @.str.int, i32 0, i32 0\n" +
          s"  $callReg = call i32 (i8*, ...) @printf(i8* $formatPtr, i32 $reg)\n"
        (st3, acc + printCode)
      }
      (finalState, code)

  private def generateBody(body: Body, state: CodeGenState): (CodeGenState, String) = {
    val (st1, declCode) = body.declarations.foldLeft((state, "")) { case ((st, acc), decl) =>
      decl match
        case VariableDeclaration(name, typeOpt, initOpt) =>
          val declaredType = typeOpt.getOrElse(IntegerType)
          val allocaType = typeToLLVMAllocaType(declaredType, st)
          val valueType = typeToLLVMType(declaredType, st)
          // First allocate the register for alloca
          val (st1, reg) = st.nextRegister()
          val st2 = st1.addVariableWithAllocaType(name, reg, valueType, allocaType, Some(declaredType))
          // Generate alloca - alloca returns a pointer, so we use the alloca type
          val allocaCode = s"  $reg = alloca $allocaType\n"
          val (st3, initCode, valueReg) = initOpt match
            case Some(expr) =>
              // Then generate the expression code
              val (st3, code, reg, regType) = generateExpression(expr, st2)
              (st3, code, reg)
            case None => 
              val zeroValue = zeroValueForType(allocaType)
              (st2, "", zeroValue)
          val zeroValue = zeroValueForType(allocaType)
          val storeCode = if initOpt.isDefined then 
            s"  store $valueType $valueReg, $allocaType* $reg\n"
          else 
            s"  store $allocaType $zeroValue, $allocaType* $reg\n"
          val code = acc + allocaCode + initCode + storeCode
          (st3, code)
        case _ => (st, acc)
    }
    val (st2, stmtCode) = body.statements.foldLeft((st1, "")) { case ((st, acc), stmt) =>
      val (st1, code) = generateStatement(stmt, st)
      (st1, acc + code)
    }
    (st2, declCode + stmtCode)
  }

  private def generateExpression(expr: Expression, state: CodeGenState): (CodeGenState, String, String, String) = expr match
    case IntegerLiteral(v) =>
      val (st1, reg) = state.nextRegister()
      (st1, s"  $reg = add i32 0, $v\n", reg, "i32")
    
    case RealLiteral(v) =>
      val (st1, reg) = state.nextRegister()
      (st1, s"  $reg = fadd double 0.0, ${v.toString}\n", reg, "double")
    
    case BooleanLiteral(v) =>
      val (st1, reg) = state.nextRegister()
      (st1, s"  $reg = add i1 0, ${if v then 1 else 0}\n", reg, "i1")
    
    case ModifiablePrimaryExpression(mp) =>
      generateModifiablePrimaryLoad(mp, state)
    
    case RoutineCallExpression(id, args) =>
      val (st1, argCode, argRegs) = args.foldLeft((state, "", List.empty[String])) { case ((st, acc, regs), arg) =>
        val (st1, code, reg, regType) = generateExpression(arg, st)
        (st1, acc + code, regs :+ reg)
      }
      val func = st1.functions.get(id)
      val retType = func.flatMap(_.returnType).map(typeToLLVMType(_, st1)).getOrElse("i32")
      val paramTypes = func.map(_.parameters.map(_.parameterType).map(typeToLLVMType(_, st1))).getOrElse(Nil)
      val (st2, callReg) = st1.nextRegister()
      val argsStr = argRegs.zip(paramTypes).map { case (r, t) => s"$t $r" }.mkString(", ")
      (st2, argCode + s"  $callReg = call $retType @$id($argsStr)\n", callReg, retType)
    
    case ParenthesizedExpression(e) =>
      generateExpression(e, state)
    
    case Relation(left, comparisons) =>
      val (st1, leftCode, leftReg, leftType) = generateSimple(left, state)
      val (st2, resultCode, resultReg, resultType) = if comparisons.isEmpty then
        // No comparisons, just return the left value as-is (not converted to boolean)
        (st1, leftCode, leftReg, leftType)
      else
        comparisons.foldLeft((st1, leftCode, leftReg, leftType)) { case ((st, acc, prevReg, prevType), (op, right)) =>
          val (st1, rightCode, rightReg, rightType) = generateSimple(right, st)
          val (st2, cmpReg) = st1.nextRegister()
          val opStr = comparisonOpToLLVM(op)
          (st2, acc + rightCode + s"  $cmpReg = icmp $opStr i32 $prevReg, $rightReg\n", cmpReg, "i1")
        }
      (st2, resultCode, resultReg, resultType)
    
    case Simple(left, operations) =>
      generateSimple(Simple(left, operations), state)
    
    case Factor(left, operations) =>
      generateFactor(Factor(left, operations), state)
    
    case Summand(primary, signOpt, isNot) =>
      val (st1, code, reg, regType) = generatePrimary(primary, state)
      val (st2, resultCode, resultReg) = if isNot then
        val (st2, notReg) = st1.nextRegister()
        (st2, code + s"  $notReg = xor i1 $reg, 1\n", notReg)
      else
        signOpt match
          case Some(Negative) =>
            val (st2, negReg) = st1.nextRegister()
            (st2, code + s"  $negReg = sub i32 0, $reg\n", negReg)
          case _ => (st1, code, reg)
      (st2, resultCode, resultReg, "i32") // Default to i32 for arithmetic operations

  private def generateSimple(simple: Simple, state: CodeGenState): (CodeGenState, String, String, String) = {
    val (st1, leftCode, leftReg, leftType) = generateFactor(simple.left, state)
    val (st2, resultCode, resultReg, resultType) = simple.operations.foldLeft((st1, leftCode, leftReg, leftType)) { case ((st, acc, accReg, accType), (op, factor)) =>
      val (st1, rightCode, rightReg, rightType) = generateFactor(factor, st)
      val (st2, opReg) = st1.nextRegister()
      val opStr = binaryOpToLLVM(op)
      // Result type is the promoted type of the operation
      val resType = if accType == "double" || rightType == "double" then "double" else if accType == "i1" || rightType == "i1" then "i1" else "i32"
      (st2, acc + rightCode + s"  $opReg = $opStr $resType $accReg, $rightReg\n", opReg, resType)
    }
    (st2, resultCode, resultReg, resultType)
  }

  private def generateFactor(factor: Factor, state: CodeGenState): (CodeGenState, String, String, String) = {
    val (st1, leftCode, leftReg, leftType) = generateSummand(factor.left, state)
    val (st2, resultCode, resultReg, resultType) = factor.operations.foldLeft((st1, leftCode, leftReg, leftType)) { case ((st, acc, accReg, accType), (op, summand)) =>
      val (st1, rightCode, rightReg, rightType) = generateSummand(summand, st)
      val (st2, opReg) = st1.nextRegister()
      val opStr = binaryOpToLLVM(op)
      val resType = if accType == "double" || rightType == "double" then "double" else if accType == "i1" || rightType == "i1" then "i1" else "i32"
      (st2, acc + rightCode + s"  $opReg = $opStr $resType $accReg, $rightReg\n", opReg, resType)
    }
    (st2, resultCode, resultReg, resultType)
  }

  private def generateSummand(summand: Summand, state: CodeGenState): (CodeGenState, String, String, String) = {
    val (st1, code, reg, regType) = generatePrimary(summand.primary, state)
    val (st2, resultCode, resultReg, resultType) = if summand.isNot then
      val (st2, notReg) = st1.nextRegister()
      (st2, code + s"  $notReg = xor i1 $reg, 1\n", notReg, "i1")
    else
      summand.sign match
        case Some(Negative) =>
          val (st2, negReg) = st1.nextRegister()
          (st2, code + s"  $negReg = sub $regType 0, $reg\n", negReg, regType)
        case _ => (st1, code, reg, regType)
    (st2, resultCode, resultReg, resultType)
  }

  private def generatePrimary(primary: Primary, state: CodeGenState): (CodeGenState, String, String, String) = primary match
    case IntegerLiteral(v) =>
      val (st1, reg) = state.nextRegister()
      (st1, s"  $reg = add i32 0, $v\n", reg, "i32")
    case RealLiteral(v) =>
      val (st1, reg) = state.nextRegister()
      (st1, s"  $reg = fadd double 0.0, ${v.toString}\n", reg, "double")
    case BooleanLiteral(v) =>
      val (st1, reg) = state.nextRegister()
      (st1, s"  $reg = add i1 0, ${if v then 1 else 0}\n", reg, "i1")
    case ModifiablePrimaryExpression(mp) =>
      generateModifiablePrimaryLoad(mp, state)
    case RoutineCallExpression(id, args) =>
      val (st1, argCode, argRegs) = args.foldLeft((state, "", List.empty[String])) { case ((st, acc, regs), arg) =>
        val (st1, code, reg, regType) = generateExpression(arg, st)
        (st1, acc + code, regs :+ reg)
      }
      val func = st1.functions.get(id)
      val retType = func.flatMap(_.returnType).map(typeToLLVMType(_, st1)).getOrElse("i32")
      val paramTypes = func.map(_.parameters.map(_.parameterType).map(typeToLLVMType(_, st1))).getOrElse(Nil)
      val (st2, callReg) = st1.nextRegister()
      val argsStr = argRegs.zip(paramTypes).map { case (r, t) => s"$t $r" }.mkString(", ")
      (st2, argCode + s"  $callReg = call $retType @$id($argsStr)\n", callReg, retType)
    case ParenthesizedExpression(e) =>
      generateExpression(e, state)

  private def generateModifiablePrimary(mp: ModifiablePrimary, state: CodeGenState): (CodeGenState, String, String) = mp match
    case node: ModifiablePrimaryNode =>
      val (st, code, reg, _) = resolveAddress(node, state)
      (st, code, reg)

  private def generateModifiablePrimaryLoad(mp: ModifiablePrimary, state: CodeGenState): (CodeGenState, String, String, String) = mp match
    case node: ModifiablePrimaryNode =>
      val (st1, addrCode, addrReg, typeOpt) = resolveAddress(node, state)
      val resolvedType = typeOpt.map(resolveTypeAliases(_, st1))
      val elemType = resolvedType.map(typeToLLVMType(_, st1)).orElse(state.variableTypes.get(node.identifier)).getOrElse("i32")
      val (st2, loadReg) = st1.nextRegister()
      val loadCode = s"  $loadReg = load $elemType, $elemType* $addrReg\n"
      (st2, addrCode + loadCode, loadReg, elemType)

  private def getModifiablePrimaryType(mp: ModifiablePrimary, state: CodeGenState): (String, String) = 
    mp match
      case ModifiablePrimaryNode(id, _, _) =>
        state.variableTypes.get(id).map(("", _)).getOrElse(("", "i32")) // Default to i32 if unknown
      case _ => ("", "i32")
  
  private def typeToLLVMType(t: Type, state: CodeGenState): String =
    resolveTypeAliases(t, state) match
      case IntegerType => "i32"
      case RealType    => "double"
      case BooleanType => "i1"
      case ArrayType(_, elementType) =>
        val elemType = typeToLLVMType(elementType, state)
        s"$elemType*"
      case record: RecordType =>
        recordToStructType(record, state)
      case TypeAlias(name) =>
        state.types.get(name).map(typeToLLVMType(_, state)).getOrElse("i32")
      case _ => "i32"
  
  // Get the value type for alloca (for arrays, return [N x T] for fixed-size arrays; for primitives, return the type itself)
  private def typeToLLVMAllocaType(t: Type, state: CodeGenState): String =
    resolveTypeAliases(t, state) match
      case IntegerType => "i32"
      case RealType    => "double"
      case BooleanType => "i1"
      case arr @ ArrayType(sizeOpt, elementType) =>
        val elemType = typeToLLVMType(elementType, state)
        sizeOpt match
          case Some(sizeExpr) =>
            extractIntLiteral(sizeExpr) match
              case Some(size) => s"[$size x $elemType]"
              case None       => s"$elemType*"
          case None => s"$elemType*"
      case record: RecordType =>
        recordToStructType(record, state)
      case TypeAlias(name) =>
        state.types.get(name).map(typeToLLVMAllocaType(_, state)).getOrElse("i32")
      case _ => "i32"

  private def binaryOpToLLVM(op: BinaryOperator): String = op match
    case Plus => "add"
    case Minus => "sub"
    case Multiply => "mul"
    case Divide => "sdiv"
    case Modulo => "srem"
    case And => "and"
    case Or => "or"
    case Xor => "xor"

  private def comparisonOpToLLVM(op: ComparisonOperator): String = op match
    case LessThan => "slt"
    case LessThanOrEqual => "sle"
    case GreaterThan => "sgt"
    case GreaterThanOrEqual => "sge"
    case Equal => "eq"
    case NotEqual => "ne"
  
  // Convert a value to boolean (i1) if it's not already
  private def ensureBoolean(valueReg: String, valueType: String, state: CodeGenState): (CodeGenState, String, String) =
    if valueType == "i1" then
      // Already boolean, use directly
      (state, "", valueReg)
    else
      // Convert to boolean by comparing with 0
      val (st1, boolReg) = state.nextRegister()
      (st1, s"  $boolReg = icmp ne $valueType $valueReg, 0\n", boolReg)

  private def resolveAddress(node: ModifiablePrimaryNode, state: CodeGenState): (CodeGenState, String, String, Option[Type]) =
    val (baseState, baseReg) = state.variables.get(node.identifier) match
      case Some(reg) => (state, reg)
      case None =>
        val (st1, allocaReg) = state.nextRegister()
        val st2 = st1.addVariableWithAllocaType(node.identifier, allocaReg, "i32", "i32")
        (st2, allocaReg)

    val baseTypeOpt    = baseState.variableTypeDefs.get(node.identifier)
    val baseAllocaOpt  = baseState.variableAllocaTypes.get(node.identifier)

    val (stateAfterMembers, memberCode, memberReg, memberTypeOpt, memberAllocaOpt) =
      node.memberAccesses.foldLeft((baseState, "", baseReg, baseTypeOpt, baseAllocaOpt)) {
        case ((st, code, currentReg, Some(currentType), _), member) =>
          resolveTypeAliases(currentType, st) match
            case record: RecordType =>
              val structType = typeToLLVMAllocaType(record, st)
              val fieldsWithIndex = record.fields.zipWithIndex
              fieldsWithIndex.find(_._1.identifier == member.identifier) match
                case Some((fieldDecl, idx)) =>
                  val fieldType = fieldDecl.typeAnnotation.getOrElse(IntegerType)
                  val fieldAllocaType = typeToLLVMAllocaType(fieldType, st)
                  val (stNext, gepReg) = st.nextRegister()
                  val gepInstr = s"  $gepReg = getelementptr inbounds $structType, $structType* $currentReg, i32 0, i32 $idx\n"
                  (stNext, code + gepInstr, gepReg, Some(fieldType), Some(fieldAllocaType))
                case None =>
                  (st, code, currentReg, None, None)
            case other =>
              (st, code, currentReg, Some(other), Some(typeToLLVMAllocaType(other, st)))
        case ((st, code, currentReg, None, allocOpt), member) =>
          (st, code, currentReg, None, allocOpt)
      }

    val (finalState, totalCode, finalReg, finalTypeOpt, finalAllocaOpt) =
      node.arrayAccesses.foldLeft((stateAfterMembers, memberCode, memberReg, memberTypeOpt, memberAllocaOpt)) {
        case ((st, code, currentReg, Some(currentType), currentAllocaOpt), ArrayAccess(indexExpr)) =>
          resolveTypeAliases(currentType, st) match
            case arrayType @ ArrayType(_, elemType) =>
              val arrayAllocaType = currentAllocaOpt.getOrElse(typeToLLVMAllocaType(arrayType, st))
              val elemAllocaType  = typeToLLVMAllocaType(elemType, st)
              val (stIdx, indexCode, indexReg, _) = generateExpression(indexExpr, st)
              val (stNext, gepReg) = stIdx.nextRegister()
              val gepInstr =
                if arrayAllocaType.startsWith("[") then
                  s"  $gepReg = getelementptr inbounds $arrayAllocaType, $arrayAllocaType* $currentReg, i32 0, i32 $indexReg\n"
                else
                  s"  $gepReg = getelementptr inbounds $elemAllocaType, $elemAllocaType* $currentReg, i32 $indexReg\n"
              (stNext, code + indexCode + gepInstr, gepReg, Some(elemType), Some(elemAllocaType))
            case other =>
              (st, code, currentReg, Some(other), currentAllocaOpt)
        case ((st, code, currentReg, None, currentAllocaOpt), ArrayAccess(indexExpr)) =>
          val (stIdx, indexCode, indexReg, _) = generateExpression(indexExpr, st)
          val (stNext, gepReg) = stIdx.nextRegister()
          val fallbackType = currentAllocaOpt.getOrElse("i32")
          val gepInstr =
            if fallbackType.startsWith("[") then
              s"  $gepReg = getelementptr inbounds $fallbackType, $fallbackType* $currentReg, i32 0, i32 $indexReg\n"
            else
              s"  $gepReg = getelementptr inbounds i32, i32* $currentReg, i32 $indexReg\n"
          (stNext, code + indexCode + gepInstr, gepReg, None, Some("i32"))
      }

    (finalState, totalCode, finalReg, finalTypeOpt)

  private def zeroValueForType(allocaType: String): String =
    if allocaType.startsWith("[") || allocaType.startsWith("{") then "zeroinitializer"
    else if allocaType.endsWith("*") then "null"
    else
      allocaType match
        case "i1"     => "0"
        case "i32"    => "0"
        case "double" => "0.0"
        case _        => "zeroinitializer"

  @tailrec
  private def resolveTypeAliases(t: Type, state: CodeGenState, seen: Set[String] = Set.empty): Type = t match
    case TypeAlias(name) if !seen.contains(name) =>
      state.types.get(name) match
        case Some(resolved) => resolveTypeAliases(resolved, state, seen + name)
        case None           => t
    case _ => t

  private def recordToStructType(record: RecordType, state: CodeGenState): String =
    val fieldTypes = record.fields.map { field =>
      val fieldType = field.typeAnnotation.getOrElse(IntegerType)
      typeToLLVMType(fieldType, state)
    }
    val inside = if fieldTypes.isEmpty then "" else fieldTypes.mkString(", ")
    s"{ $inside }"

  private def extractIntLiteral(expr: Expression): Option[Int] = expr match
    case parser.structures.IntegerLiteral(value) => Some(value)
    case parser.structures.Summand(primary, signOpt, _) =>
      extractPrimaryLiteral(primary).map { base =>
        signOpt match
          case Some(Negative) => -base
          case _              => base
      }
    case parser.structures.Factor(left, ops) if ops.isEmpty => extractIntLiteral(left)
    case parser.structures.Simple(left, ops) if ops.isEmpty => extractIntLiteral(left)
    case parser.structures.Relation(left, comps) if comps.isEmpty => extractIntLiteral(left)
    case parser.structures.ParenthesizedExpression(inner) => extractIntLiteral(inner)
    case _ => None

  private def extractPrimaryLiteral(primary: Primary): Option[Int] = primary match
    case parser.structures.IntegerLiteral(value) => Some(value)
    case parser.structures.ParenthesizedExpression(inner) => extractIntLiteral(inner)
    case _ => None
}

