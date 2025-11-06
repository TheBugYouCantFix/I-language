package parser

import lexer.Token
import parser.structures.*
import parser.parsers.ParserState
import lexer.TokenType

type ParserError = Unit // TODO: implement parser error
type ParseResult[R] = Either[ParserError, (ParserState, R)]

extension [A](p: (ParserState, A))
    def map[B](f: A => B): (ParserState, B) = p match
        case (s, a) => (s, f(a))

object SyntaxAnalyzer:
    def analyze(tokens: List[Token]): Program =
        import Parser.*
        @annotation.tailrec
        def loop(state: ParserState, declsAcc: List[Declaration], stmtsAcc: List[Statement]): (ParserState, List[Declaration]) =
            // Try routine declaration first
            parseRoutineDeclaration(state) match
                case Right((s1, rd)) =>
                    val withStmts = if stmtsAcc.nonEmpty then StatementDeclaration(stmtsAcc.reverse) :: declsAcc else declsAcc
                    loop(s1, rd :: withStmts, Nil)
                case Left(_) =>
                    // Try simple declaration (var/type)
                    parseSimpleDeclaration(state) match
                        case Right((s2, sd)) =>
                            val withStmts = if stmtsAcc.nonEmpty then StatementDeclaration(stmtsAcc.reverse) :: declsAcc else declsAcc
                            loop(s2, sd :: withStmts, Nil)
                        case Left(_) =>
                            // Try statement
                            parseStatement(state) match
                                case Right((s3, st)) => loop(s3, declsAcc, st :: stmtsAcc)
                                case Left(_) => (state, (if stmtsAcc.nonEmpty then StatementDeclaration(stmtsAcc.reverse) :: declsAcc else declsAcc))

        val (_, decls) = loop(ParserState(tokens), Nil, Nil)
        Program(decls.reverse)

    object Parser:
        private def peekAndCheck(state: ParserState)(p: Token => Boolean) = 
            state.peek match
                case Some(t) => Right(p(t))
                case None    => Left(())

        private def discardSpecific(state: ParserState)(p: Token => Boolean): Either[ParserError, ParserState] =
            peekAndCheck(state)(p).flatMap {
                b => if b then Right(state.discardN()) else Left(())
            }
        
        private def discardSpecificN(state: ParserState)(ps: List[Token => Boolean]): Either[ParserError, ParserState] =
            ps match
                case p :: ps => discardSpecific(state)(p).flatMap {
                    s => discardSpecificN(s)(ps)
                }
                case Nil => Right(state)

        def parseSimpleDeclaration(state: ParserState): ParseResult[SimpleDeclaration] = 
            parseVariableDeclaration(state) orElse parseTypeDeclaration(state)
        
        def parseVariableDeclaration(state: ParserState): ParseResult[VariableDeclaration] =
            state.advanceN(2) match
                case (
                    List(
                        Token(TokenType.Var, _, _),
                        Token(TokenType.Identifier, idName, _),
                    ),
                    nextState
                ) => 
                    nextState.advanceN() match
                        case (Token(TokenType.Is, _, _) :: Nil, nextState) =>
                            parseExpression(nextState).map {
                                _.map(e => VariableDeclaration(idName, None, Some(e)))
                            }
                        case (Token(TokenType.Colon, _, _) :: Nil, nextState) =>
                            for
                                (s, varType) <- parseType(nextState)
                                (s, initOpt) <- peekAndCheck(s)(_.tkType == TokenType.Is).flatMap {
                                    case true  => parseExpression(s.discardN()).map(_.map(Some(_)))
                                    case false => Right((s, None))
                                }
                            yield (s, VariableDeclaration(idName, Some(varType), initOpt))
                        case _ => Left(())
                case _ => Left(())

        def parseTypeDeclaration(state: ParserState): ParseResult[TypeDeclaration] = 
            state.advanceN(3) match
                case (
                    List(
                        Token(TokenType.Type, _, _),
                        Token(TokenType.Identifier, idName, _),
                        Token(TokenType.Is, _, _)
                    ),
                    nextState
                ) =>
                    parseType(nextState).map {
                        _.map(type_ => TypeDeclaration(idName, type_))
                    }
                case _ => Left(())
        
        def parseType(state: ParserState): ParseResult[Type] =
            def parseRecordType(acc: List[VariableDeclaration], state: ParserState): Either[ParserError, (ParserState, List[VariableDeclaration])] = {
                state.peek match
                    case Some(Token(TokenType.End, _, _)) =>
                        Right((state.discardN(), acc))
                    case Some(_) => parseVariableDeclaration(state).flatMap {
                        case (nextState, varDecl) => parseRecordType(varDecl :: acc, nextState)
                    }
                    case None => Left(())
            }

            state.advanceN() match
                case (Token(TokenType.Integer, _, _) :: Nil, nextState) => Right((nextState, IntegerType))
                case (Token(TokenType.Real, _, _) :: Nil, nextState) => Right((nextState, RealType))
                case (Token(TokenType.Boolean, _, _) :: Nil, nextState) => Right((nextState, BooleanType))
                case (Token(TokenType.Record, _, _) :: Nil, nextState) =>
                    parseRecordType(Nil, nextState).map(_.map(RecordType(_)))
                case (Token(TokenType.Array, _, _) :: Nil, nextState) =>    
                    nextState.advanceN() match
                        case (Token(TokenType.LeftBracket, _, _) :: Nil, afterLBracket) =>
                            afterLBracket.peek match
                                case Some(Token(TokenType.RightBracket, _, _)) =>
                                    parseType(afterLBracket.discardN()).map {
                                        _.map(type_ => ArrayType(None, type_))
                                    }
                                case Some(_) => 
                                    for 
                                        (afterSize, size) <- parseExpression(afterLBracket)
                                        isNextBracket     <- peekAndCheck(afterSize)(_.tkType == TokenType.RightBracket)
                                        (s, type_)        <- if isNextBracket then parseType(afterSize.discardN()) else Left(())
                                    yield (s, ArrayType(Some(size), type_))
                                case None => Left(())
                        case _ => Left(())
                case (Token(TokenType.Identifier, idName, _) :: Nil, nextState) =>
                    Right((nextState, TypeAlias(idName)))
                case _ => Left(())

        def parseStatement(state: ParserState): ParseResult[Statement] =         
            parseAssignment(state)  orElse
            parseRoutineCall(state) orElse
            parseWhileLoop(state)   orElse
            parseForLoop(state)     orElse
            parseIfStatement(state) orElse
            parsePrintStatement(state)

        def parseAssignment(state: ParserState): ParseResult[Assignment] =
            for 
                (s, m) <- parseModifiablePrimary(state)
                s      <- discardSpecific(s)(_.tkType == TokenType.Assignment)
                (s, e) <- parseExpression(s)
            yield (s, Assignment(m, e))
        
        def parseRoutineCall(state: ParserState): ParseResult[RoutineCall] = 
            def parseArguments(state: ParserState, acc: List[Expression]): Either[ParserError, (ParserState, List[Expression])] =
                state.peek match
                    case Some(Token(TokenType.RightParen, _, _)) => Right((state.discardN(), acc))
                    case Some(Token(TokenType.Comma, _, _)) =>
                        if acc == Nil then Left(()) else parseExpression(state.discardN()).flatMap {
                            case (s, e) => parseArguments(s, e :: acc)
                        }
                    case Some(_) => Left(())
                    case None => Left(())

            state.advanceN() match
                case (Token(TokenType.Identifier, idName, _) :: Nil, nextState) =>
                    peekAndCheck(nextState)(_.tkType == TokenType.LeftParen).flatMap {
                        case true =>
                            val afterL = nextState.discardN()
                            peekAndCheck(afterL)(_.tkType == TokenType.RightParen).flatMap {
                                case true => Right((afterL.discardN(), RoutineCall(idName, Nil)))
                                case false =>
                                    for
                                        (s, firstArg) <- parseExpression(afterL)
                                        (s, args)     <- parseArguments(s, firstArg :: Nil)
                                    yield (s, RoutineCall(idName, args))
                            }
                        case false => Left(())
                    }
                case _ => Left(())
            
        def parseWhileLoop(state: ParserState): ParseResult[WhileLoop] = 
            for 
                s      <- discardSpecific(state)(_.tkType == TokenType.While)
                (s, e) <- parseExpression(s)
                s      <- discardSpecific(s)(_.tkType == TokenType.Loop)
                (s, b) <- parseBody(s)
                s      <- discardSpecific(s)(_.tkType == TokenType.End)
            yield (s, WhileLoop(e, b))
        
        def parseForLoop(state: ParserState): ParseResult[ForLoop] = 
            for
                s <- discardSpecific(state)(_.tkType == TokenType.For)
                (s, idName) <- s.advanceN() match
                    case (Token(TokenType.Identifier, idName, _) :: Nil, s) => Right((s, idName))
                    case _ => Left(())
                s <- discardSpecific(s)(_.tkType == TokenType.In)
                (s, range) <- parseRange(s)
                (s, isReverse) <- peekAndCheck(s)(_.tkType == TokenType.Reverse).map {
                    case true  => (s.discardN(), true)
                    case false => (s, false)
                }
                s <- discardSpecific(s)(_.tkType == TokenType.Loop)
                (s, body) <- parseBody(s)
                s <- discardSpecific(s)(_.tkType == TokenType.End)
            yield (s, ForLoop(idName, range, isReverse, body))
        
        def parseModifiablePrimary(state: ParserState): ParseResult[ModifiablePrimary] =
            state.advanceN() match
                case (Token(TokenType.Identifier, idName, _) :: Nil, nextState) =>
                    @annotation.tailrec
                    def loop(s: ParserState, members: List[MemberAccess], arrays: List[ArrayAccess]): (ParserState, List[MemberAccess], List[ArrayAccess]) =
                        s.peek match
                            case Some(Token(TokenType.Dot, _, _)) =>
                                s.discardN().advanceN() match
                                    case (Token(TokenType.Identifier, mem, _) :: Nil, ns) =>
                                        loop(ns, MemberAccess(mem) :: members, arrays)
                                    case _ => (s, members, arrays)
                            case Some(Token(TokenType.LeftBracket, _, _)) =>
                                parseExpression(s.discardN()) match
                                    case Right((ns, idx)) =>
                                        ns.peek match
                                            case Some(Token(TokenType.RightBracket, _, _)) => loop(ns.discardN(), members, ArrayAccess(idx) :: arrays)
                                            case _ => (s, members, arrays)
                                    case Left(_) => (s, members, arrays)
                            case _ => (s, members, arrays)
                    val (endState, mems, arrs) = loop(nextState, Nil, Nil)
                    Right((endState, ModifiablePrimaryNode(idName, mems.reverse, arrs.reverse)))
                case _ => Left(())
        
        def parseRange(state: ParserState): ParseResult[Range] =
            for 
                (s, firstVar) <- parseExpression(state)
                (s, secVar)   <- {
                    peekAndCheck(s)(_.tkType == TokenType.RangeOp).flatMap {
                        case true  => parseExpression(s.discardN()).map(_.map(Some(_)))
                        case false =>
                            s.peek match
                                case Some(Token(TokenType.Dot, _, _)) =>
                                    val afterFirstDot = s.discardN()
                                    afterFirstDot.peek match
                                        case Some(Token(TokenType.Dot, _, _)) =>
                                            parseExpression(afterFirstDot.discardN()).map(_.map(Some(_)))
                                        case _ => Right((s, None))
                                case _ => Right((s, None))
                    }
                }
            yield (s, Range(firstVar, secVar))

        def parseIfStatement(state: ParserState): ParseResult[IfStatement] = 
            for 
                s <- discardSpecific(state)(_.tkType == TokenType.If)
                (s, e) <- parseExpression(s)
                s <- discardSpecific(s)(_.tkType == TokenType.Then)
                (s, body) <- parseBody(s)
                (s, elseBody) <- peekAndCheck(s)(_.tkType == TokenType.Else).flatMap {
                    case true  => parseBody(s.discardN()).map(_.map(Some(_)))
                    case false => Right((s, None))
                }
                s <- discardSpecific(s)(_.tkType == TokenType.End)
            yield (s, IfStatement(e, body, elseBody))
        
        def parsePrintStatement(state: ParserState): ParseResult[PrintStatement] =
            def parseArguments(state: ParserState, acc: List[Expression]): ParseResult[List[Expression]] =
                state.peek match
                    case None => Right((state, acc))  // End of input, return accumulated args
                    case Some(Token(TokenType.Comma, _, _)) =>
                        parseExpression(state.discardN()).flatMap {
                            case (s, e) => parseArguments(s, e :: acc)
                        }
                    case Some(_) => Right((state, acc))  // No comma, done parsing args

            for 
                s <- discardSpecific(state)(_.tkType == TokenType.Print)
                (s, firstArg) <- parseExpression(s)
                (s, args) <- parseArguments(s, firstArg :: Nil)
            yield (s, PrintStatement(args))
        
        def parseRoutineDeclaration(state: ParserState): ParseResult[RoutineDeclaration] =
            for
                (s, rHead) <- parseRoutineHeader(state)
                b <- peekAndCheck(s)(_.tkType == TokenType.Is)
                (s, rBody) <- if b then parseRoutineBody(s).map(_.map(Some(_))) else Right((s, None))
            yield (s, RoutineDeclaration(rHead, rBody))

        def parseRoutineHeader(state: ParserState): ParseResult[RoutineHeader] =
            state.advanceN(3) match
                case (
                    List(
                        Token(TokenType.Routine, _, _),
                        Token(TokenType.Identifier, idName, _),
                        Token(TokenType.LeftParen, _, _)
                    ),
                    nextState
                ) =>
                    // Allow empty parameter list
                    peekAndCheck(nextState)(_.tkType == TokenType.RightParen).flatMap {
                        case true =>
                            val afterR = nextState.discardN()
                            for
                                b <- peekAndCheck(afterR)(_.tkType == TokenType.Colon)
                                (s, type_) <- if b then parseType(afterR.discardN()).map(_.map(Some(_))) else Right((afterR, None))
                            yield (s, RoutineHeader(idName, Nil, type_))
                        case false =>
                            for
                                (s, params) <- parseParameters(nextState)
                                s <- discardSpecific(s)(_.tkType == TokenType.RightParen)
                                b <- peekAndCheck(s)(_.tkType == TokenType.Colon)
                                (s, type_) <- if b then parseType(s.discardN()).map(_.map(Some(_))) else Right((s, None))
                            yield (s, RoutineHeader(idName, params, type_))
                    }
                case _ => Left(())

        def parseRoutineBody(state: ParserState): ParseResult[RoutineBody] =
            state.advanceN() match
                case (Token(TokenType.Is, _, _) :: Nil, nextState) =>
                    // After 'is' accept either a block body or a single expression followed by 'end'
                    parseBody(nextState) match
                        case Right((sBody, body)) =>
                            discardSpecific(sBody)(_.tkType == TokenType.End).map { sEnd => (sEnd, JustRoutineBody(body)) }
                        case Left(_) =>
                            for
                                (sExpr, e) <- parseExpression(nextState)
                                sEnd       <- discardSpecific(sExpr)(_.tkType == TokenType.End)
                            yield (sEnd, RoutineBodyExpression(e))
                case (Token(TokenType.Gteq, _, _) :: Nil, nextState) =>
                    parseExpression(nextState).map {
                        _.map(e => RoutineBodyExpression(e))
                    }
                case _ => Left(())

        def parseParameters(state: ParserState): ParseResult[List[ParameterDeclaration]] = 
            def loop(state: ParserState, acc: List[ParameterDeclaration]): ParseResult[List[ParameterDeclaration]] =
                peekAndCheck(state)(_.tkType == TokenType.Comma) match
                    case Left(_) => Left(()) 
                    case Right(true) => parseParameterDeclaration(state.discardN()).flatMap {
                        case (s, paramDecl) => loop(s, paramDecl :: acc)
                    }
                    case Right(false) => Right((state, acc))
            
            parseParameterDeclaration(state).flatMap {
                case (s, paramDecl) => loop(s, paramDecl :: Nil)
            }

        def parseParameterDeclaration(state: ParserState): ParseResult[ParameterDeclaration] =
            state.advanceN(2) match
                case (
                    List(
                        Token(TokenType.Identifier, idName, _),
                        Token(TokenType.Colon, _, _)
                    ),
                    nextState
                ) =>
                    parseType(nextState).map {
                        _.map(type_ => ParameterDeclaration(idName, type_))
                    }
                case _ => Left(())

        def parseBody(state: ParserState): ParseResult[Body] = 
            def loop(state: ParserState, simpleDecls: List[SimpleDeclaration], statements: List[Statement]): ParseResult[Body] =
                parseSimpleDeclaration(state) orElse parseStatement(state) match
                    case Left(_) =>
                        if simpleDecls == Nil && statements == Nil then Left(())
                        else Right((state, Body(simpleDecls, statements)))
                    case Right((nextState, s: Statement)) => loop(nextState, simpleDecls, s :: statements)
                    case Right((nextState, s: SimpleDeclaration)) => loop(nextState, s :: simpleDecls, statements)
            
            loop(state, Nil, Nil)
                    
        def parseExpression(state: ParserState): ParseResult[Expression] =
            parseRelation(state)

        def parseRelation(state: ParserState): ParseResult[Relation] =
            def comparisonOf(t: TokenType): Option[ComparisonOperator] = t match
                case TokenType.Lt   => Some(LessThan)
                case TokenType.Lteq => Some(LessThanOrEqual)
                case TokenType.Gt   => Some(GreaterThan)
                case TokenType.Gteq => Some(GreaterThanOrEqual)
                case TokenType.Eq   => Some(Equal)
                case TokenType.Neq  => Some(NotEqual)
                case _ => None

            def loop(s: ParserState, left: Simple, acc: List[(ComparisonOperator, Simple)]): ParseResult[Relation] =
                s.peek match
                    case Some(Token(tk, _, _)) =>
                        comparisonOf(tk) match
                            case Some(op) =>
                                parseSimple(s.discardN()).flatMap { case (ns, right) => loop(ns, left, (op -> right) :: acc) }
                            case None => Right((s, Relation(left, acc.reverse)))
                    case None => Right((s, Relation(left, acc.reverse)))

            parseSimple(state).flatMap { case (s, left) => loop(s, left, Nil) }

        def parseSimple(state: ParserState): ParseResult[Simple] =
            def binOpOf(t: TokenType): Option[BinaryOperator] = t match
                case TokenType.Plus => Some(Plus)
                case TokenType.Minus => Some(Minus)
                case TokenType.Or => Some(Or)
                case TokenType.Xor => Some(Xor)
                case _ => None

            def loop(s: ParserState, left: Factor, acc: List[(BinaryOperator, Factor)]): ParseResult[Simple] =
                s.peek match
                    case Some(Token(tk, _, _)) =>
                        binOpOf(tk) match
                            case Some(op) =>
                                parseFactor(s.discardN()).flatMap { case (ns, right) => loop(ns, left, (op -> right) :: acc) }
                            case None => Right((s, Simple(left, acc.reverse)))
                    case None => Right((s, Simple(left, acc.reverse)))

            parseFactor(state).flatMap { case (s, left) => loop(s, left, Nil) }

        def parseFactor(state: ParserState): ParseResult[Factor] =
            def binOpOf(t: TokenType): Option[BinaryOperator] = t match
                case TokenType.Mul => Some(Multiply)
                case TokenType.Div => Some(Divide)
                case TokenType.Mod => Some(Modulo)
                case TokenType.And => Some(And)
                case _ => None

            def loop(s: ParserState, left: Summand, acc: List[(BinaryOperator, Summand)]): ParseResult[Factor] =
                s.peek match
                    case Some(Token(tk, _, _)) =>
                        binOpOf(tk) match
                            case Some(op) =>
                                parseSummand(s.discardN()).flatMap { case (ns, right) => loop(ns, left, (op -> right) :: acc) }
                            case None => Right((s, Factor(left, acc.reverse)))
                    case None => Right((s, Factor(left, acc.reverse)))

            parseSummand(state).flatMap { case (s, left) => loop(s, left, Nil) }

        def parseSummand(state: ParserState): ParseResult[Summand] =
            val (signOpt, afterSign) = parseSign(state) match
                case Right((s, sg)) => (Some(sg), s)
                case Left(_) => (None, state)

            val (isNot, afterNot) = peekAndCheck(afterSign)(_.tkType == TokenType.Not) match
                case Right(true) => (true, afterSign.discardN())
                case _           => (false, afterSign)

            parsePrimary(afterNot).map { case (s, p) => (s, Summand(p, signOpt, isNot)) }

        def parsePrimary(state: ParserState): ParseResult[Primary] =
            state.peek match
                case Some(Token(TokenType.IntegerLiteral, v, _)) => Right((state.discardN(), IntegerLiteral(v.toInt)))
                case Some(Token(TokenType.RealLiteral, v, _))    => Right((state.discardN(), RealLiteral(v.toDouble)))
                case Some(Token(TokenType.True, _, _))           => Right((state.discardN(), BooleanLiteral(true)))
                case Some(Token(TokenType.False, _, _))          => Right((state.discardN(), BooleanLiteral(false)))
                case Some(Token(TokenType.LeftParen, _, _)) =>
                    parseExpression(state.discardN()).flatMap { case (s, e) =>
                        peekAndCheck(s)(_.tkType == TokenType.RightParen).map {
                            case true  => (s.discardN(), ParenthesizedExpression(e))
                            case false => (s, ParenthesizedExpression(e)) // ill-formed, but keep state
                        }
                    }
                case Some(Token(TokenType.Identifier, _, _)) =>
                    // Prefer modifiable primary unless followed by '('
                    peekAndCheck(state.advanceN()._2)(_.tkType == TokenType.LeftParen) match
                        case Right(true) =>
                            parseRoutineCall(state).map { case (s, rc) => (s, RoutineCallExpression(rc.identifier, rc.arguments)) }
                        case _ =>
                            parseModifiablePrimary(state).map { case (s, mp) => (s, ModifiablePrimaryExpression(mp)) }
                case _ => Left(())

        def parseSign(state: ParserState): ParseResult[Sign] =
            state.peek match
                case Some(Token(TokenType.Plus, _, _))  => Right((state.discardN(), Positive))
                case Some(Token(TokenType.Minus, _, _)) => Right((state.discardN(), Negative))
                case _ => Left(())
