package parser

import lexer.Token
import parser.structures.*
import parser.parsers.ParserState
import cats.syntax.either.*
import lexer.TokenType

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
            parseVariableDeclaration(state).leftFlatMap(_ => parseTypeDeclaration(state))
        
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

        def parseAssignment(state: ParserState): ParseResult[Assignment] =
            for 
                (s, m) <- parseModifiablePrimary(state)
                s      <- discardSpecific(s)(_.tkType == TokenType.Assignment)
                (s, e) <- parseExpression(s)
            yield (s, Assignment(m, e))
        
        def parseRoutineCall(state: ParserState): ParseResult[RoutineCall] = 
            def parseArguments(state: ParserState, acc: List[Expression]): Either[ParserError, List[Expression]] =
                peekAndCheck(state)(_.tkType == TokenType.RightBrace).flatMap {
                    b => if b then Right(acc) else parseArguments(state.discardN(), )
                }
                    
                
                
                

            state.advanceN() match
                case (Token(TokenType.Identifier, idName, _) :: Nil, nextState) =>
                    peekAndCheck(nextState)(_.tkType == TokenType.LeftBrace).flatMap {
                        b => if b then 
                    }
                    

            
            
        def parseModifiablePrimary(state: ParserState): ParseResult[ModifiablePrimary] = ???                    
        
        def parseExpression(state: ParserState): ParseResult[Expression] = ???

        
