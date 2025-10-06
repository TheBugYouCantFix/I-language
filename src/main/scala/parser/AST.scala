package parser

case class Program(
    topDeclarations: Vector[SimpleDeclaration | RoutineDeclaration]
)
    

