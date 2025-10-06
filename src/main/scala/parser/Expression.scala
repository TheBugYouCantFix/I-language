package parser

sealed trait Expression

case class UnRelation(rel: Relation) extends Expression

case class And(rel1: Relation, rel2: Relation) extends Expression

case class Or(rel1: Relation, rel2: Relation) extends Expression

case class Xor(rel1: Relation, rel2: Relation) extends Expression

sealed trait Relation

case class UnSimple(simple: Simple) extends Relation

case class <(simple1: Simple, simple2: Simple) extends Relation

case class <=(simple1: Simple, simple2: Simple) extends Relation

case class >(simple1: Simple, simple2: Simple) extends Relation

case class >=(simple1: Simple, simple2: Simple) extends Relation

case class `=`(simple1: Simple, simple2: Simple) extends Relation

case class /=(simple1: Simple, simple2: Simple) extends Relation

sealed trait Simple

case class UnFactor(factor: Factor) extends Simple

case class *(factor1: Factor, factor2: Factor) extends Simple

case class /(factor1: Factor, factor2: Factor) extends Simple

case class %(factor1: Factor, factor2: Factor) extends Simple

sealed trait Factor

case class UnSummand(summand: Summand) extends Factor

case class +(summand1: Summand, summand2: Summand) extends Factor

case class -(summand1: Summand, summand2: Summand) extends Factor

sealed trait Summand

sealed trait Primary extends Summand

case class 

case class ExpSummand(expression: Expression) extends Summand



