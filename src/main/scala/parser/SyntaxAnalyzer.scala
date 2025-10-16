package parser

import lexer.Token
import parser.structures.*
import parser.parsers.ParserState
import cats.syntax.either.*
import lexer.TokenType
import cats.syntax.arrow
import scala.quoted.Expr
import scala.util.CommandLineParser.ParseError

type ParserError = Unit // TODO: implement parser error
type ParseResult[R] = Either[ParserError, (ParserState, R)]

extension [A](p: (ParserState, A))
    def map[B](f: A => B): (ParserState, B) = p match
        case (s, a) => (s, f(a))

object SyntaxAnalyzer:
    def analyze(tokens: List[Token]): Program = ???

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
                                s            <- discardSpecific(s)(_.tkType == TokenType.Is)
                                (s, e)       <- parseExpression(s)
                            yield (s, VariableDeclaration(idName, Some(varType), Some(e)))
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
                    case Some(_) => parseVariableDeclaration(state.discardN()).flatMap {
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
                        case (Token(TokenType.LeftBracket, _, _) :: Nil, nextState) =>
                            nextState.peek match
                                case Some(Token(TokenType.RightBracket, _, _)) =>
                                    parseType(nextState.discardN()).map {
                                        _.map(type_ => ArrayType(None, type_))
                                    }
                                case Some(_) => 
                                    for 
                                        (nextState, size) <- parseExpression(nextState.advanceN()._2)
                                        isNextBracket     <- peekAndCheck(nextState)(_.tkType == TokenType.RightBracket)
                                        (s, type_)        <- if isNextBracket then parseType(nextState.discardN()) else Left(())
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
                    case Some(Token(TokenType.RightBrace, _, _)) => Right((state.discardN(), acc))
                    case Some(Token(TokenType.Comma, _, _)) =>
                        if acc == Nil then Left(()) else parseExpression(state.discardN()).flatMap {
                            case (s, e) => parseArguments(s, e :: acc)
                        }
                    case Some(_) => Left(())
                    case None => Left(())

            state.advanceN() match
                case (Token(TokenType.Identifier, idName, _) :: Nil, nextState) =>
                    // TODO: unchecked token peek!!!
                    peekAndCheck(nextState)(_.tkType == TokenType.LeftBrace).flatMap {
                        case true =>
                            for
                                (s, firstArg) <- parseExpression(nextState.discardN())
                                (s, args)     <- parseArguments(s, firstArg :: Nil)
                            yield (s, RoutineCall(idName, args))
                        case false => Right((nextState, RoutineCall(idName, Nil)))
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
        
        def parseModifiablePrimary(state: ParserState): ParseResult[ModifiablePrimary] = ???
        
        def parseRange(state: ParserState): ParseResult[Range] =
            for 
                (s, firstVar) <- parseExpression(state)
                (s, secVar)   <- peekAndCheck(s)(_.tkType == TokenType.RangeOp).flatMap {
                    case true  => parseExpression(s.discardN()).map(_.map(Some(_)))
                    case false => Right((s, None))
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
                peekAndCheck(state)(_.tkType == TokenType.Comma) match
                    case Left(_) => Left(()) 
                    case Right(true) => parseExpression(state.discardN()).flatMap {
                        case (s, e) => parseArguments(s, e :: acc)
                    }
                    case Right(false) => Right((state, acc))

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
                        Token(TokenType.LeftBrace, _, _)
                    ),
                    nextState
                ) =>
                    for
                        (s, params) <- parseParameters(nextState)
                        s <- discardSpecific(s)(_.tkType == TokenType.RightBrace)
                        b <- peekAndCheck(s)(_.tkType == TokenType.Colon)
                        (s, type_) <- if b then parseType(s.discardN()).map(_.map(Some(_))) else Right((s, None))
                    yield (s, RoutineHeader(idName, params, type_))
                case _ => Left(())

        def parseRoutineBody(state: ParserState): ParseResult[RoutineBody] =
            state.advanceN() match
                case (Token(TokenType.Is, _, _) :: Nil, nextState) =>
                    for 
                        (s, body) <- parseBody(nextState)
                        s <- discardSpecific(s)(_.tkType == TokenType.End)
                    yield (s, JustRoutineBody(body))
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
                    
        def parseExpression(state: ParserState): ParseResult[Expression] = ???

        def parseRelation(state: ParserState): ParseResult[Relation] = ???

        def parseSimple(state: ParserState): ParseResult[Simple] = ???

        def parseFactor(state: ParserState): ParseResult[Factor] = ???

        def parseSummand(state: ParserState): ParseResult[Summand] = ???

        def parsePrimary(state: ParserState): ParseResult[Primary] = ???

        def parseSign(state: ParserState): ParseResult[Sign] = ???
