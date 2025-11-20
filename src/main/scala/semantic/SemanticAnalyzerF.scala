package semantic

import parser.structures.*
import cats.syntax.all.*
import cats.data.State
import scala.annotation.tailrec

class SemanticError(message: String) extends RuntimeException(message)

sealed trait SymbolInfo
case class VariableInfo(name: String, varType: Option[Type], isInitialized: Boolean, isUsed: Boolean) extends SymbolInfo
case class RoutineInfo(name: String, parameters: List[ParameterDeclaration], returnType: Option[Type], isDefined: Boolean) extends SymbolInfo
case class TypeInfo(name: String, typeDefinition: Type) extends SymbolInfo

case class SemanticContext(
  isInLoop: Boolean = false,
  isInunction: Boolean = false,
  currentFunctionReturnType: Option[Type] = None
)

case class SymbolTable(
  variables: Map[String, VariableInfo] = Map.empty,
  routines: Map[String, RoutineInfo] = Map.empty,
  types: Map[String, TypeInfo] = Map.empty
) {
  def lookupVariable(name: String): Option[VariableInfo] = variables.get(name)
  def lookupRoutine(name: String): Option[RoutineInfo] = routines.get(name)
  def lookupType(name: String): Option[TypeInfo] = types.get(name)

  def addVariable(info: VariableInfo): SymbolTable = copy(variables = variables.updated(info.name, info))
  def addRoutine(info: RoutineInfo): SymbolTable = copy(routines = routines.updated(info.name, info))
  def addType(info: TypeInfo): SymbolTable = copy(types = types.updated(info.name, info))

  def markVariableUsed(name: String): SymbolTable =
    variables.get(name) match
      case Some(v) => copy(variables = variables.updated(name, v.copy(isUsed = true)))
      case None    => this
}

case class SemanticResult(
  errors: List[SemanticError],
  optimizedProgram: Option[Program] = None
)

private case class SemState(table: SymbolTable, errors: Vector[SemanticError]) {
  def addError(e: SemanticError): SemState = copy(errors = errors :+ e)
  def withTable(t: SymbolTable): SemState = copy(table = t)
}

object SemanticAnalyzer {
  private type SemS[A] = State[SemState, A]

  def analyze(program: Program): SemanticResult = {
    val context = SemanticContext()
    val initial = SemState(SymbolTable(), Vector.empty)

    val afterCollect = collectDeclarationsS(program, context).runS(initial).value
    val afterChecks  = checkProgramS(program, context).runS(afterCollect).value
    val optimized    = optimize(program, afterChecks.table)

    SemanticResult(afterChecks.errors.toList, Some(optimized))
  }

  // Pass 1: collect declarations (State-based driver)
  private def collectDeclarationsS(program: Program, context: SemanticContext): SemS[Unit] =
    program.declarations.foldLeft(State.pure[SemState, Unit](())) { (acc, d) =>
      acc.flatMap(_ => State.modify(st => collectDeclaration(d, st, context)))
    }

  // Pass 2: semantic checks (State-based driver)
  private def checkProgramS(program: Program, context: SemanticContext): SemS[Unit] =
    program.declarations.foldLeft(State.pure[SemState, Unit](())) { (acc, d) =>
      acc.flatMap(_ => State.modify(st => checkDeclaration(d, st, context)))
    }

  // Pass 1: collect declarations
  private def collectDeclaration(decl: Declaration, state: SemState, context: SemanticContext): SemState = decl match
    case VariableDeclaration(name, tOpt, initOpt) =>
      if state.table.variables.contains(name) then state.addError(SemanticError(s"Duplicate variable declaration: '$name'"))
      else
        // inferring type in case it was not specified explicitly
//        val (st, inferredT) = (tOpt, initOpt) match
//          case (None, Some(expr)) => inferType(expr, state, context)
//          case _ => (state, tOpt)
        state.withTable(state.table.addVariable(VariableInfo(name, tOpt, initOpt.isDefined, isUsed = false)))

    case TypeDeclaration(name, typeDef) =>
      if state.table.types.contains(name) then state.addError(SemanticError(s"Duplicate type declaration: '$name'"))
      else state.withTable(state.table.addType(TypeInfo(name, typeDef)))

    case RoutineDeclaration(header, _) =>
      if state.table.routines.contains(header.identifier) then state.addError(SemanticError(s"Duplicate routine declaration: '${header.identifier}'"))
      else state.withTable(state.table.addRoutine(RoutineInfo(header.identifier, header.parameters, header.returnType, isDefined = true)))

    case StatementDeclaration(_) => state

  // Pass 2: semantic checks
  private def checkDeclaration(decl: Declaration, state: SemState, context: SemanticContext): SemState = decl match
    case VariableDeclaration(name, tOpt, initOpt) =>
      val st1 = tOpt.fold(state)(t => checkType(t, state))
      initOpt match
        case Some(init) =>
          val (st2, initType) = inferType(init, st1, context)
          val st2b = if tOpt.isEmpty && initType.isDefined then
            st2.withTable(st2.table.addVariable(
              st2.table.lookupVariable(name).get.copy(varType = initType)
            ))
          else st2
          val st3 = tOpt.fold(st2b) { declared =>
            if !areTypesCompatible(declared, initType, st2b.table) then st2b.addError(SemanticError(s"Type mismatch: variable '$name' declared as ${typeToString(declared)} but initialized with ${typeToStringOpt(initType)}"))
            else st2b
          }
          checkExpression(init, st3, context, markUsage = true)
        case None => st1

    case TypeDeclaration(name, typeDef) =>
      val st1 = checkType(typeDef, state)
      checkRecursiveType(name, typeDef, st1)

    case RoutineDeclaration(header, bodyOpt) =>
      val newContext = context.copy(isInunction = true, currentFunctionReturnType = header.returnType)
      val stParams = header.parameters.foldLeft(state) { (st, p) =>
        val stWith = st.withTable(st.table.addVariable(VariableInfo(p.identifier, Some(p.parameterType), isInitialized = true, isUsed = false)))
        checkType(p.parameterType, stWith)
      }
      bodyOpt match
        case Some(body) =>
          val stBody = checkRoutineBody(body, stParams, newContext)
          val (_, bodyType) = inferRoutineBodyType(body, stBody, newContext)
          header.returnType match
            case Some(ret) => if !areTypesCompatible(ret, bodyType, stBody.table) then stBody.addError(SemanticError(s"Return type mismatch in routine '${header.identifier}': declared ${typeToString(ret)} but body returns ${typeToStringOpt(bodyType)}")) else stBody
            case None => stBody
        case None => stParams

    case StatementDeclaration(statements) =>
      statements.foldLeft(state) { (st, s) => checkStatement(s, st, context) }

  private def checkRoutineBody(body: RoutineBody, state: SemState, context: SemanticContext): SemState = body match
    case JustRoutineBody(b) =>
      val stDecls = b.declarations.foldLeft(state) { (st, d) => checkDeclaration(d, st, context) }
      b.statements.foldLeft(stDecls) { (st, s) => checkStatement(s, st, context) }
    case RoutineBodyExpression(expr) => checkExpression(expr, state, context, markUsage = false)

  private def checkStatement(stmt: Statement, state: SemState, context: SemanticContext): SemState = stmt match
    case Assignment(target, value) =>
      val st1 = checkModPrimary(target, state, context, markUsage = true)
      val (st2, targetType) = inferModPrimaryType(target, st1, context)
      val (st3, valueType)  = inferType(value, st2, context)
      targetType match
        case Some(tt) => if !areTypesCompatible(tt, valueType, st3.table) then st3.addError(SemanticError(s"Type mismatch in assignment: target is ${typeToString(tt)} but value is ${typeToStringOpt(valueType)}")) else st3
        case None => st3

    case RoutineCall(id, args) =>
      state.table.lookupRoutine(id) match
        case Some(r) =>
          if r.parameters.length != args.length then state.addError(SemanticError(s"Wrong number of arguments in call to '$id': expected ${r.parameters.length}, got ${args.length}"))
          else args.zip(r.parameters).foldLeft(state) { case (st, (arg, param)) =>
            val (st2, at) = inferType(arg, st, context)
            if !areTypesCompatible(param.parameterType, at, st2.table) then st2.addError(SemanticError(s"Type mismatch in argument to '$id': parameter '${param.identifier}' expects ${typeToString(param.parameterType)} but got ${typeToStringOpt(at)}")) else st2
          }
        case None => state.addError(SemanticError(s"Undeclared routine: '$id'"))

    case WhileLoop(cond, body) =>
      val (st1, ct) = inferType(cond, state, context)
      val st2 = if !isOfType(BooleanType, ct, st1.table) then st1.addError(SemanticError("While loop condition must be boolean")) else st1
      checkBody(body, st2, context.copy(isInLoop = true))

    case ForLoop(loopVar, range, _, body) =>
      val (st1, tStart) = inferType(range.start, state, context)
      val (st2, tEnd)   = range.end.map(inferType(_, st1, context)).getOrElse((st1, None))
      val st3 = if !isOfType(IntegerType, tStart, st2.table) then st2.addError(SemanticError("for loop start value must be integer")) else st2
      val st4 = tEnd.fold(st3)(t => if !isOfType(IntegerType, Some(t), st3.table) then st3.addError(SemanticError("for loop end value must be integer")) else st3)
      val stWithVar = st4.withTable(st4.table.addVariable(VariableInfo(loopVar, Some(IntegerType), isInitialized = true, isUsed = false)))
      checkBody(body, stWithVar, context.copy(isInLoop = true))

    case IfStatement(cond, thenB, elseB) =>
      val (st1, ct) = inferType(cond, state, context)
      val st2 = if !isOfType(BooleanType, ct, st1.table) then st1.addError(SemanticError("If statement condition must be boolean")) else st1
      val stThen = checkBody(thenB, st2, context)
      elseB.fold(stThen) { eb => checkBody(eb, stThen, context) }

    case PrintStatement(values) =>
      values.foldLeft(state) { (st, e) => checkExpression(e, st, context, markUsage = true) }

  private def checkBody(body: Body, state: SemState, context: SemanticContext): SemState = {
    val stDecls = body.declarations.foldLeft(state) { (st, d) => checkDeclaration(d, st, context) }
    body.statements.foldLeft(stDecls) { (st, s) => checkStatement(s, st, context) }
  }

  private def checkExpression(expr: Expression, state: SemState, context: SemanticContext, markUsage: Boolean): SemState = {
    def validateExpr(e: Expression, st: SemState): SemState = e match
      case RoutineCallExpression(id, args) =>
        st.table.lookupRoutine(id) match
          case Some(r) =>
            val stCount = if r.parameters.length != args.length then st.addError(SemanticError(s"Wrong number of arguments in call to '$id': expected ${r.parameters.length}, got ${args.length}")) else st
            args.zipAll(r.parameters, null, null).foldLeft(stCount) {
              case (s0, (arg, param)) if arg != null && param != null =>
                val (s1, at) = inferType(arg, s0, context)
                if !areTypesCompatible(param.parameterType, at, s1.table) then s1.addError(SemanticError(s"Type mismatch in argument to '$id': parameter '${param.identifier}' expects ${typeToString(param.parameterType)} but got ${typeToStringOpt(at)}")) else s1
              case (s0, (arg, _)) if arg != null =>
                val (s1, _) = inferType(arg, s0, context); s1
              case (s0, _) => s0
            }
          case None =>
            val sUnd = st.addError(SemanticError(s"Undeclared routine: '$id'"))
            args.foldLeft(sUnd) { (s0, a) => val (s1, _) = inferType(a, s0, context); s1 }
      case ParenthesizedExpression(inner) => validateExpr(inner, st)
      case Relation(left, comps) =>
        val st1 = validateExpr(left, st)
        comps.foldLeft(st1) { case (s0, (_, sm)) => validateExpr(sm, s0) }
      case Simple(left, ops) =>
        val st1 = validateExpr(left, st)
        ops.foldLeft(st1) { case (s0, (_, f)) => validateExpr(f, s0) }
      case Factor(left, ops) =>
        val st1 = validateExpr(left, st)
        ops.foldLeft(st1) { case (s0, (_, sm)) => validateExpr(sm, s0) }
      case Summand(primary, _, _) => validateExpr(primary, st)
      case _ => st

    val (st1, _) = inferType(expr, state, context)
    val st2 = validateExpr(expr, st1)
    if markUsage then markUsageInExpr(expr, st2, context) else st2
  }

  private def markUsageInExpr(expr: Expression, state: SemState, context: SemanticContext): SemState = expr match
    case ModifiablePrimaryExpression(mp) =>
      mp match
        case ModifiablePrimaryNode(id, _, _) => state.withTable(state.table.markVariableUsed(id))
    case RoutineCallExpression(_, args)  => args.foldLeft(state) { (st, e) => markUsageInExpr(e, st, context) }
    case ParenthesizedExpression(e)      => markUsageInExpr(e, state, context)
    case Relation(l, comps)              => comps.foldLeft(markUsageInExpr(l, state, context)) { case (st, (_, s)) => markUsageInExpr(s, st, context) }
    case Simple(l, ops)                  => ops.foldLeft(markUsageInExpr(l, state, context)) { case (st, (_, f)) => markUsageInExpr(f, st, context) }
    case Factor(l, ops)                  => ops.foldLeft(markUsageInExpr(l, state, context)) { case (st, (_, sm)) => markUsageInExpr(sm, st, context) }
    case Summand(p, _, _)                => markUsageInExpr(p, state, context)
    case IfStatement(cond, thenB, elseB) =>
      val st1 = checkExpression(cond, state, context, markUsage = true)
      val (st2, ct) = inferType(cond, st1, context)
      val st3 = if !isOfType(BooleanType, ct, st2.table) then st2.addError(SemanticError("If statement condition must be boolean")) else st2
      val stThen = checkBody(thenB, st3, context)
      elseB.fold(stThen) { eb => checkBody(eb, stThen, context) }
    case WhileLoop(cond, body) =>
      val st1 = checkExpression(cond, state, context, markUsage = true)
      val (st2, ct) = inferType(cond, st1, context)
      val st3 = if !isOfType(BooleanType, ct, st2.table) then st2.addError(SemanticError("While loop condition must be boolean")) else st2
      checkBody(body, st3, context.copy(isInLoop = true))
    case ForLoop(loopVar, range, _, body) =>
      val st1 = checkExpression(range.start, state, context, markUsage = true)
      val (st2, tStart) = inferType(range.start, st1, context)
      val (st3, tEnd) = range.end.map { endExpr =>
        val stWithCheck = checkExpression(endExpr, st2, context, markUsage = true)
        inferType(endExpr, stWithCheck, context)
      }.getOrElse((st2, None))
      val st4 = if !isOfType(IntegerType, tStart, st3.table) then st3.addError(SemanticError("for loop start value must be integer")) else st3
      val st5 = tEnd.fold(st4)(t => if !isOfType(IntegerType, Some(t), st4.table) then st4.addError(SemanticError("for loop end value must be integer")) else st4)
      val stWithVar = st5.withTable(st5.table.addVariable(VariableInfo(loopVar, Some(IntegerType), isInitialized = true, isUsed = false)))
      checkBody(body, stWithVar, context.copy(isInLoop = true))
    case PrintStatement(values) =>
      values.foldLeft(state) { (st, e) => checkExpression(e, st, context, markUsage = true) }
    case _                               => state

  private def checkModPrimary(mp: ModifiablePrimary, state: SemState, context: SemanticContext, markUsage: Boolean): SemState = mp match
    case ModifiablePrimaryNode(id, _, arrayAccesses) =>
      val st1 = state.table.lookupVariable(id) match
        case Some(_) => if markUsage then state.withTable(state.table.markVariableUsed(id)) else state
        case None    => state.addError(SemanticError(s"Undeclared variable: '$id'"))
      arrayAccesses.foldLeft(st1) { (st, access) =>
        val (st2, tIndex) = inferType(access.index, st, context)
        val st3 = if !isOfType(IntegerType, tIndex, st2.table) then st2.addError(SemanticError("Array index must be integer")) else st2
        checkArrayBounds(access.index, st3)
      }

  @tailrec
  private def resolveTypeAlias(t: Type, table: SymbolTable): Type = t match {
    case TypeAlias(name) =>
      table.lookupType(name).map(_.typeDefinition) match
        case Some(resolved) => resolveTypeAlias(resolved, table)
        case None           => t
    case _ => t
  }

  private def inferType(expr: Expression, state: SemState, context: SemanticContext): (SemState, Option[Type]) = expr match
    case IntegerLiteral(_)  => (state, Some(IntegerType))
    case RealLiteral(_)     => (state, Some(RealType))
    case BooleanLiteral(_)  => (state, Some(BooleanType))
    case ModifiablePrimaryExpression(mp) => inferModPrimaryType(mp, state, context)
    case RoutineCallExpression(id, _)    => state.table.lookupRoutine(id) match
      case Some(r) => (state, r.returnType)
      case None    => (state, None)  // Don't report error here - validateExpr will handle it
    case ParenthesizedExpression(e)      => inferType(e, state, context)
    case Relation(left, Nil)             => inferType(left, state, context)
    case Relation(_, _)                  => (state, Some(BooleanType))
    case Simple(left, ops) =>
      val (st1, lt) = inferType(left, state, context)
      ops.foldLeft((st1, lt)) { case ((st, ct), (op, f)) =>
        val (st2, rt) = inferType(f, st, context)
        val resolvedLeft = ct.map(t => resolveTypeAlias(t, st2.table))
        val resolvedRight = rt.map(t => resolveTypeAlias(t, st2.table))
        (resolvedLeft, resolvedRight) match
          case (Some(t1), Some(t2)) if isNumericType(t1, st2.table) && isNumericType(t2, st2.table) => (st2, promoteNumeric(Some(t1), Some(t2)))
          case (Some(BooleanType), Some(BooleanType))                                                => (st2, Some(BooleanType))
          case (Some(t1), Some(t2)) => (st2.addError(SemanticError(s"Invalid operation: ${binOpToString(op)} applied to ${typeToString(t1)} and ${typeToString(t2)}")), None)
          case _ => (st2.addError(SemanticError(s"Invalid operation: ${binOpToString(op)} applied to incompatible types")), None)
      }
    case Factor(left, ops) =>
      val (st1, lt) = inferType(left, state, context)
      ops.foldLeft((st1, lt)) { case ((st, ct), (op, sm)) =>
        val (st2, rt) = inferType(sm, st, context)
        (ct, rt) match
          case (Some(t1), Some(t2)) if isNumericType(t1, st2.table) && isNumericType(t2, st2.table) => (st2, promoteNumeric(Some(t1), Some(t2)))
          case (Some(BooleanType), Some(BooleanType))                                                => (st2, Some(BooleanType))
          case (Some(t1), Some(t2)) => (st2.addError(SemanticError(s"Invalid operation: ${binOpToString(op)} applied to ${typeToString(t1)} and ${typeToString(t2)}")), None)
          case _ => (st2.addError(SemanticError(s"Invalid operation: ${binOpToString(op)} applied to incompatible types")), None)
      }
    case Summand(primary, signOpt, isNot) =>
      val (st1, pt) = inferType(primary, state, context)
      if isNot then
        if isOfType(BooleanType, pt, st1.table) then (st1, Some(BooleanType)) else (st1.addError(SemanticError("Not operator can only be applied to boolean")), None)
      else (st1, pt)

  private def inferModPrimaryType(mp: ModifiablePrimary, state: SemState, context: SemanticContext): (SemState, Option[Type]) = mp match
    case ModifiablePrimaryNode(id, memberAccesses, arrayAccesses) =>
      state.table.lookupVariable(id) match

        case None =>
          (state.addError(SemanticError(s"Undeclared variable: '$id'")), None)
        case Some(varInfo) => varInfo.varType match
          case Some(varType) =>
            val (stMembers, memberTypeOpt) =
              memberAccesses.foldLeft((state, Some(varType): Option[Type])) { case ((st, currentTypeOpt), member) =>
                currentTypeOpt match
                  case Some(currentType) =>
                    resolveTypeAlias(currentType, st.table) match
                      case record: RecordType =>
                        record.fields.find(_.identifier == member.identifier) match
                          case Some(fieldDecl) =>
                            val fieldType = fieldDecl.typeAnnotation.getOrElse(IntegerType)
                            (st, Some(fieldType))
                          case None =>
                            (st.addError(SemanticError(s"Type '${typeToString(currentType)}' has no member '${member.identifier}'")), None)
                      case TypeAlias(name) =>
                        (st.addError(SemanticError(s"Undeclared type: '$name'")), None)
                      case _ =>
                        (st.addError(SemanticError(s"Type '${typeToString(currentType)}' has no member '${member.identifier}'")), None)
                  case None => (st, None)
              }

            arrayAccesses.foldLeft((stMembers, memberTypeOpt)) { case ((st, ct), access) =>
              ct match
                case Some(currentType) =>
                  resolveTypeAlias(currentType, st.table) match
                    case ArrayType(sizeOpt, elemT) =>
                      (evaluateConstant(access.index, st.table), sizeOpt.flatMap(evaluateConstant(_, st.table))) match {
                        case (Some(idx), Some(size)) if idx < 0 || idx >= size =>
                          (st.addError(SemanticError(s"Array index out of bounds: $idx for array of size $size")), Some(elemT))
                        case _ => (st, Some(elemT))
                      }
                    case TypeAlias(name) =>
                      (st.addError(SemanticError(s"Undeclared type: '$name'")), None)
                    case _ =>
                      (st.addError(SemanticError("Cannot index non-array type")), None)
                case None => (st, None)
            }
          case None => (state.addError(SemanticError(s"Undeclared variable: '$id'")), None)

  private def inferRoutineBodyType(body: RoutineBody, state: SemState, context: SemanticContext): (SemState, Option[Type]) = body match
    case JustRoutineBody(_)      => (state, None)
    case RoutineBodyExpression(e) => inferType(e, state, context)

  private def areTypesCompatible(expected: Type, actual: Option[Type], symbolTable: SymbolTable): Boolean = actual match
    case Some(a) => areTypesCompatible(expected, a, symbolTable)
    case None    => false

  private def areTypesCompatible(expected: Type, actual: Type, symbolTable: SymbolTable): Boolean = (expected, actual) match
    case (IntegerType, RealType) => true
    case (RealType, IntegerType) => false
    case (t1, t2) if t1 == t2    => true
    case (TypeAlias(n1), TypeAlias(n2)) if n1 == n2 => true
    case (TypeAlias(n), other)   => symbolTable.lookupType(n).exists(ti => areTypesCompatible(ti.typeDefinition, other, symbolTable))
    case (other, TypeAlias(n))   => symbolTable.lookupType(n).exists(ti => areTypesCompatible(other, ti.typeDefinition, symbolTable))
    case (ArrayType(_, e1), ArrayType(_, e2)) => areTypesCompatible(e1, e2, symbolTable)
    case _ => false

  private def isOfType(typeToCheck: Type, tOpt: Option[Type], st: SymbolTable): Boolean =
    tOpt.exists {
      case t if t == typeToCheck => true
      case TypeAlias(n) => st.lookupType(n).exists(ti => isOfType(typeToCheck, Some(ti.typeDefinition), st))
      case _ => false
    }

  private def isNumericType(value: Type, table: SymbolTable): Boolean = {
    val tOpt = Some(value)
    isOfType(IntegerType, tOpt, table) || isOfType(RealType, tOpt, table)
  }

  private def promoteNumeric(t1: Option[Type], t2: Option[Type]): Option[Type] = (t1, t2) match
    case (Some(RealType), _) | (_, Some(RealType)) => Some(RealType)
    case (Some(IntegerType), Some(IntegerType))     => Some(IntegerType)
    case _ => None

  private def checkArrayBounds(indexExpr: Expression, state: SemState): SemState = evaluateConstant(indexExpr, state.table) match
    case Some(i) if i < 0 => state.addError(SemanticError(s"Array index must be non-negative, got $i"))
    case _ => state

  private def checkType(t: Type, state: SemState): SemState = t match
    case _: PrimitiveType => state
    case ArrayType(sizeOpt, elem) =>
      val st1 = sizeOpt.fold(state) { sizeExpr =>
        val (stx, tSize) = inferType(sizeExpr, state, SemanticContext())
        val sty = if !isOfType(IntegerType, tSize, stx.table) then stx.addError(SemanticError("Array size must be integer")) else stx
        evaluateConstant(sizeExpr, sty.table) match
          case Some(sz) if sz <= 0 => sty.addError(SemanticError(s"Array size must be positive, got $sz"))
          case _ => sty
      }
      checkType(elem, st1)
    case RecordType(fields) => fields.foldLeft(state) { (st, f) => checkType(f.typeAnnotation.getOrElse(IntegerType), st) }
    case TypeAlias(name) => if !state.table.types.contains(name) then state.addError(SemanticError(s"Undeclared type: '$name'")) else state

  private def checkRecursiveType(typeName: String, typeDef: Type, state: SemState): SemState = typeDef match
    case TypeAlias(n) if n == typeName => state.addError(SemanticError(s"Recursive type alias: '$typeName' directly references itself"))
    case _ => state

  // Constant evaluation (subset)
  private def evaluateConstant(expr: Expression, table: SymbolTable): Option[Int] = expr match
    case Relation(left, Nil) => evaluateConstant(left, table)
    case Relation(_, _)      => None
    case IntegerLiteral(v) => Some(v)
    case ParenthesizedExpression(e) => evaluateConstant(e, table)
    case Simple(left, ops) =>
      evaluateConstant(left, table).flatMap { lv =>
        ops.foldLeft(Some(lv): Option[Int]) { (curr, op) =>
          for
            c <- curr
            rv <- evaluateConstant(op._2, table)
            res <- op._1 match
              case Plus   => Some(c + rv)
              case Minus  => Some(c - rv)
              case _      => None
          yield res
        }
      }
    case Factor(left, ops) =>
      evaluateConstant(left, table).flatMap { lv =>
        ops.foldLeft(Some(lv): Option[Int]) { (curr, op) =>
          for
            c <- curr
            rv <- evaluateConstant(op._2, table)
            res <- op._1 match
              case Multiply       => Some(c * rv)
              case Divide  if rv != 0 => Some(c / rv)
              case Modulo  if rv != 0 => Some(c % rv)
              case _ => None
          yield res
        }
      }
    case Summand(primary, signOpt, _) =>
      evaluateConstant(primary, table).map { v => signOpt match
        case Some(Negative) => -v
        case _ => v
      }
    case _ => None

  // Pass 3: Optimizations (pure)
  private def optimize(program: Program, table: SymbolTable): Program = {
    val optimizedDecls = program.declarations.map(optimizeDeclaration(_, table))
    val declarationsWithUsage = optimizedDecls.flatMap {
      case VariableDeclaration(name, tOpt, initOpt) =>
        if table.variables.get(name).exists(!_.isUsed) then None else Some(VariableDeclaration(name, tOpt, initOpt))
      case other => Some(other)
    }
    Program(declarationsWithUsage)
  }

  private def optimizeDeclaration(decl: Declaration, table: SymbolTable): Declaration = decl match
    case VariableDeclaration(name, tOpt, initOpt) => VariableDeclaration(name, tOpt, initOpt.map(optimizeExpression(_, table)))
    case TypeDeclaration(name, t)                 => TypeDeclaration(name, optimizeType(t, table))
    case RoutineDeclaration(h, b)                 => RoutineDeclaration(h, b.map(optimizeRoutineBody(_, table)))
    case StatementDeclaration(stmts)              => StatementDeclaration(stmts.map(optimizeStatement(_, table)))

  private def optimizeExpression(expr: Expression, table: SymbolTable): Expression = expr match
    case s: Simple   => optimizeSimple(s, table)
    case f: Factor   => optimizeFactor(f, table)
    case r: Relation => optimizeRelation(r, table)
    case s: Summand  => optimizeSummand(s, table)
    case ParenthesizedExpression(e) => ParenthesizedExpression(optimizeExpression(e, table))
    case other => other

  private def optimizeRelation(expr: Relation, table: SymbolTable): Relation =
    val left2 = optimizeSimple(expr.left, table)
    val comps2 = expr.comparisons.map { case (op, s) => (op, optimizeSimple(s, table)) }
    Relation(left2, comps2)

  private def intToFactor(v: Int): Factor = Factor(Summand(IntegerLiteral(v), None, isNot = false), Nil)
  private def intToSimple(v: Int): Simple = Simple(intToFactor(v), Nil)

  private def optimizeSimple(simple: Simple, table: SymbolTable): Simple =
    val left2 = optimizeFactor(simple.left, table)
    val ops2  = simple.operations.map { case (op, f) => (op, optimizeFactor(f, table)) }
    evaluateConstant(Simple(left2, ops2), table) match
      case Some(v) => intToSimple(v)
      case None    => Simple(left2, ops2)

  private def optimizeFactor(fFactor: Factor, table: SymbolTable): Factor =
    val left2 = optimizeSummand(fFactor.left, table)
    val ops2  = fFactor.operations.map { case (op, sm) => (op, optimizeSummand(sm, table)) }
    evaluateConstant(Factor(left2, ops2), table) match
      case Some(v) => intToFactor(v)
      case None    => Factor(left2, ops2)

  private def optimizeSummand(summand: Summand, table: SymbolTable): Summand =
    val p2 = optimizePrimary(summand.primary, table)
    Summand(p2, summand.sign, summand.isNot)

  private def optimizePrimary(primary: Primary, table: SymbolTable): Primary = primary match
    case ModifiablePrimaryExpression(mp) => ModifiablePrimaryExpression(mp)
    case RoutineCallExpression(id, args) => RoutineCallExpression(id, args.map(optimizeExpression(_, table)))
    case ParenthesizedExpression(e)      => optimizeExpression(e, table) match
      case lit: LiteralValue => lit
      case other => ParenthesizedExpression(other)
    case lit: LiteralValue => lit

  private def optimizeStatement(stmt: Statement, table: SymbolTable): Statement = stmt match
    case Assignment(t, v)      => Assignment(t, optimizeExpression(v, table))
    case RoutineCall(id, args) => RoutineCall(id, args.map(optimizeExpression(_, table)))
    case WhileLoop(c, b)       => WhileLoop(optimizeExpression(c, table), optimizeBody(b, table))
    case ForLoop(v, r, rev, b) => ForLoop(v, Range(optimizeExpression(r.start, table), r.end.map(optimizeExpression(_, table))), rev, optimizeBody(b, table))
    case IfStatement(c, tb, eb) => IfStatement(optimizeExpression(c, table), optimizeBody(tb, table), eb.map(optimizeBody(_, table)))
    case PrintStatement(values) => PrintStatement(values.map(optimizeExpression(_, table)))

  private def optimizeRoutineBody(body: RoutineBody, table: SymbolTable): RoutineBody = body match
    case JustRoutineBody(b)       => JustRoutineBody(optimizeBody(b, table))
    case RoutineBodyExpression(e) => RoutineBodyExpression(optimizeExpression(e, table))

  private def optimizeBody(body: Body, table: SymbolTable): Body =
    Body(body.declarations.map(optimizeDeclaration(_, table).asInstanceOf[SimpleDeclaration]), body.statements.map(optimizeStatement(_, table)))

  private def optimizeType(t: Type, table: SymbolTable): Type = t match
    case ArrayType(size, elem) => ArrayType(size.map(optimizeExpression(_, table)), optimizeType(elem, table))
    case RecordType(fields)    => RecordType(fields.map(f => VariableDeclaration(f.identifier, f.typeAnnotation.map(optimizeType(_, table)), f.initializer.map(optimizeExpression(_, table)))))
    case other => other

  private def typeToString(t: Type): String = t match
    case IntegerType => "integer"
    case RealType    => "real"
    case BooleanType => "boolean"
    case ArrayType(_, el) => s"array ${typeToString(el)}"
    case RecordType(_) => "record"
    case TypeAlias(n)  => n

  private def typeToStringOpt(t: Option[Type]): String = t.map(typeToString).getOrElse("unknown")

  private def binOpToString(op: BinaryOperator): String = op match
    case Plus => "+"
    case Minus => "-"
    case Multiply => "*"
    case Divide => "/"
    case Modulo => "%"
    case And => "and"
    case Or => "or"
    case Xor => "xor"
}
