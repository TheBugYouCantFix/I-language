package parser.structures

sealed trait ASTNode
trait Declaration extends ASTNode
trait Statement extends ASTNode
trait Expression extends ASTNode
trait Type extends ASTNode
trait ModifiablePrimary extends ASTNode
trait Literal extends ASTNode
trait BinaryOperator extends ASTNode
trait ComparisonOperator extends ASTNode
trait LogicalOperator extends BinaryOperator