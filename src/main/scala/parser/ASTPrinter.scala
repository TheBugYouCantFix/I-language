package parser

import parser.structures.*

object ASTPrinter:
  def format(program: Program): String =
    val sb = new StringBuilder
    appendProgram(sb, program, "")
    sb.toString

  private def nl(sb: StringBuilder, indent: String, line: String): Unit =
    sb.append(indent).append(line).append("\n")

  private def appendProgram(sb: StringBuilder, program: Program, indent: String): Unit =
    nl(sb, indent, "Program")
    program.declarations.foreach(d => appendDeclaration(sb, d, indent + "  "))

  private def appendDeclaration(sb: StringBuilder, d: Declaration, indent: String): Unit = d match
    case TypeDeclaration(id, t) =>
      nl(sb, indent, s"TypeDeclaration: $id")
      appendType(sb, t, indent + "  ")
    case VariableDeclaration(id, tOpt, init) =>
      nl(sb, indent, s"VariableDeclaration: $id")
      tOpt.foreach(t => { nl(sb, indent + "  ", "Type:"); appendType(sb, t, indent + "    ") })
      init.foreach(e => { nl(sb, indent + "  ", "Initializer:"); appendExpression(sb, e, indent + "    ") })
    case RoutineDeclaration(h, b) =>
      nl(sb, indent, s"RoutineDeclaration: ${h.identifier}")
      nl(sb, indent + "  ", "Header:")
      appendRoutineHeader(sb, h, indent + "    ")
      b.foreach { body =>
        nl(sb, indent + "  ", "Body:")
        appendRoutineBody(sb, body, indent + "    ")
      }
    case StatementDeclaration(stmts) =>
      nl(sb, indent, s"StatementDeclaration")
      stmts.foreach(s => appendStatement(sb, s, indent + "  "))

  private def appendRoutineHeader(sb: StringBuilder, h: RoutineHeader, indent: String): Unit =
    nl(sb, indent, s"Parameters:")
    h.parameters.foreach { p =>
      nl(sb, indent + "  ", s"Param: ${p.identifier}")
      appendType(sb, p.parameterType, indent + "    ")
    }
    h.returnType.foreach { t =>
      nl(sb, indent, "ReturnType:")
      appendType(sb, t, indent + "  ")
    }

  private def appendRoutineBody(sb: StringBuilder, b: RoutineBody, indent: String): Unit = b match
    case JustRoutineBody(body) => appendBody(sb, body, indent)
    case RoutineBodyExpression(e) =>
      nl(sb, indent, "Expression:")
      appendExpression(sb, e, indent + "  ")

  private def appendBody(sb: StringBuilder, body: Body, indent: String): Unit =
    nl(sb, indent, "Body")
    if body.declarations.nonEmpty then
      nl(sb, indent + "  ", "Declarations:")
      body.declarations.foreach(d => appendDeclaration(sb, d, indent + "    "))
    if body.statements.nonEmpty then
      nl(sb, indent + "  ", "Statements:")
      body.statements.foreach(s => appendStatement(sb, s, indent + "    "))

  private def appendStatement(sb: StringBuilder, s: Statement, indent: String): Unit = s match
    case Assignment(target, value) =>
      nl(sb, indent, "Assignment")
      nl(sb, indent + "  ", "Target:")
      appendModPrimary(sb, target, indent + "    ")
      nl(sb, indent + "  ", "Value:")
      appendExpression(sb, value, indent + "    ")
    case RoutineCall(id, args) =>
      nl(sb, indent, s"RoutineCall: $id")
      args.foreach(a => appendExpression(sb, a, indent + "  "))
    case WhileLoop(cond, body) =>
      nl(sb, indent, "WhileLoop")
      nl(sb, indent + "  ", "Condition:")
      appendExpression(sb, cond, indent + "    ")
      appendBody(sb, body, indent + "  ")
    case ForLoop(loopVar, range, isReverse, body) =>
      nl(sb, indent, s"ForLoop${if isReverse then " (reverse)" else ""}")
      nl(sb, indent + "  ", s"Var: $loopVar")
      nl(sb, indent + "  ", "Range:")
      appendRange(sb, range, indent + "    ")
      appendBody(sb, body, indent + "  ")
    case IfStatement(cond, thenB, elseB) =>
      nl(sb, indent, "If")
      nl(sb, indent + "  ", "Condition:")
      appendExpression(sb, cond, indent + "    ")
      nl(sb, indent + "  ", "Then:")
      appendBody(sb, thenB, indent + "    ")
      elseB.foreach(b => { nl(sb, indent + "  ", "Else:"); appendBody(sb, b, indent + "    ") })
    case PrintStatement(values) =>
      nl(sb, indent, "Print")
      values.foreach(v => appendExpression(sb, v, indent + "  "))
    
    case ReturnStatement(value) =>
      nl(sb, indent, "Return")
      appendExpression(sb, value, indent + "  ")

  private def appendRange(sb: StringBuilder, r: Range, indent: String): Unit =
    nl(sb, indent, "Range")
    nl(sb, indent + "  ", "Start:")
    appendExpression(sb, r.start, indent + "    ")
    r.end.foreach { e =>
      nl(sb, indent + "  ", "End:")
      appendExpression(sb, e, indent + "    ")
    }

  private def appendExpression(sb: StringBuilder, e: Expression, indent: String): Unit = e match
    case r: Relation => appendRelation(sb, r, indent)
    case s: Simple   => appendSimple(sb, s, indent)
    case f: Factor   => appendFactor(sb, f, indent)
    case s: Summand  => appendSummand(sb, s, indent)
    case p: Primary  => appendPrimary(sb, p, indent)

  private def appendRelation(sb: StringBuilder, r: Relation, indent: String): Unit =
    nl(sb, indent, "Relation")
    appendSimple(sb, r.left, indent + "  ")
    r.comparisons.foreach { (op, s) =>
      nl(sb, indent + "  ", s"${opToString(op)}:")
      appendSimple(sb, s, indent + "    ")
    }

  private def appendSimple(sb: StringBuilder, s: Simple, indent: String): Unit =
    nl(sb, indent, "Simple")
    appendFactor(sb, s.left, indent + "  ")
    s.operations.foreach { (op, f) =>
      nl(sb, indent + "  ", s"${binOpToString(op)}:")
      appendFactor(sb, f, indent + "    ")
    }

  private def appendFactor(sb: StringBuilder, f: Factor, indent: String): Unit =
    nl(sb, indent, "Factor")
    appendSummand(sb, f.left, indent + "  ")
    f.operations.foreach { (op, sm) =>
      nl(sb, indent + "  ", s"${binOpToString(op)}:")
      appendSummand(sb, sm, indent + "    ")
    }

  private def appendSummand(sb: StringBuilder, s: Summand, indent: String): Unit =
    val prefix = (if s.sign.contains(Positive) then "+" else if s.sign.contains(Negative) then "-" else "") + (if s.isNot then " not" else "")
    nl(sb, indent, s"Summand${if prefix.nonEmpty then s" [$prefix]" else ""}")
    appendPrimary(sb, s.primary, indent + "  ")

  private def appendPrimary(sb: StringBuilder, p: Primary, indent: String): Unit = p match
    case IntegerLiteral(v) => nl(sb, indent, s"Int($v)")
    case RealLiteral(v)    => nl(sb, indent, s"Real($v)")
    case BooleanLiteral(v) => nl(sb, indent, s"Bool($v)")
    case ParenthesizedExpression(e) =>
      nl(sb, indent, s"Parens")
      appendExpression(sb, e, indent + "  ")
    case RoutineCallExpression(id, args) =>
      nl(sb, indent, s"RoutineCallExpr: $id")
      args.foreach(a => appendExpression(sb, a, indent + "  "))
    case ModifiablePrimaryExpression(mp) =>
      nl(sb, indent, s"ModPrimary")
      appendModPrimary(sb, mp, indent + "  ")

  private def appendModPrimary(sb: StringBuilder, mp: ModifiablePrimary, indent: String): Unit = mp match
    case ModifiablePrimaryNode(id, members, arrays) =>
      nl(sb, indent, s"Id: $id")
      members.foreach { m => nl(sb, indent + "  ", s". ${m.identifier}") }
      arrays.foreach { a =>
        nl(sb, indent + "  ", s"[")
        appendExpression(sb, a.index, indent + "    ")
        nl(sb, indent + "  ", s"]")
      }

  private def appendType(sb: StringBuilder, t: Type, indent: String): Unit = t match
    case IntegerType => nl(sb, indent, "IntegerType")
    case RealType    => nl(sb, indent, "RealType")
    case BooleanType => nl(sb, indent, "BooleanType")
    case TypeAlias(id) => nl(sb, indent, s"TypeAlias($id)")
    case RecordType(fields) =>
      nl(sb, indent, "RecordType")
      fields.foreach(f => appendDeclaration(sb, f, indent + "  "))
    case ArrayType(size, elem) =>
      nl(sb, indent, "ArrayType")
      size.foreach { s => nl(sb, indent + "  ", "Size:"); appendExpression(sb, s, indent + "    ") }
      nl(sb, indent + "  ", "ElementType:")
      appendType(sb, elem, indent + "    ")

  private def opToString(op: ComparisonOperator): String = op match
    case LessThan => "<"
    case LessThanOrEqual => "<="
    case GreaterThan => ">"
    case GreaterThanOrEqual => ">="
    case Equal => "=="
    case NotEqual => "!="

  private def logicalOpToString(op: LogicalOperator): String = op match
    case Or => "or"
    case And => "and"
    case Xor => "xor"

  private def binOpToString(op: BinaryOperator): String = op match
    case Plus => "+"
    case Minus => "-"
    case Multiply => "*"
    case Divide => "/"
    case Modulo => "%"
    case And => "and"
    case Or => "or"
    case Xor => "xor"




