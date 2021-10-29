package de.mlte.propositional

sealed abstract class Expression
case class Conjunction(a: Expression, b: Expression) extends Expression
case class Disjunction(a: Expression, b: Expression) extends Expression
case class Negation(a: Expression) extends Expression
case class Value(value: Boolean) extends Expression
case class Variable(name: String) extends Expression

object Expression {
    def level(e: Expression): Int = e match {
        case _: Disjunction => 0
        case _: Conjunction => 1
        case _: Negation => 2
        case _: Value | _: Variable => 3
    }

    def brackets(outer: Expression, content: Expression): String = {
        val innerLevel = level(content)
        val outerLevel = level(outer)
        val c = print(content)
        if (innerLevel >= outerLevel) c else "(" + c + ")"
    }

    def print(e: Expression): String = e match {
        case Conjunction(a, b) => brackets(e, a) + " && " + brackets(e, b)
        case Disjunction(a, b) => brackets(e, a) + " || " + brackets(e, b)
        case Negation(a) => "!" + brackets(e, a)
        case Value(a) => a.toString
        case Variable(a) => a
    }

    def nnf(e: Expression): Expression = e match {
        case Conjunction(a, b) => Conjunction(nnf(a), nnf(b))
        case Disjunction(a, b) => Disjunction(nnf(a), nnf(b))
        case v: Value => v
        case v: Variable => v
        case Negation(Conjunction(a,b)) => Conjunction(nnf(Negation(a)), nnf(Negation(b)))
        case Negation(Disjunction(a,b)) => Disjunction(nnf(Negation(a)), nnf(Negation(b)))
        case Negation(Negation(x)) => x
        case Negation(v: Value) => Negation(v)
        case Negation(a: Variable) => Negation(a)
    }

    def assign(e: Expression, world: Set[String]) = {
        def ass(e: Expression): Expression = e match {
            case Conjunction(a, b) => Conjunction(ass(a), ass(b))
            case Disjunction(a, b) => Disjunction(ass(a), ass(b))
            case Negation(a) => Negation(ass(a))
            case v: Value => v
            case Variable(name) => Value(world(name))
        }
        ass(e)
    }

    def eval(e: Expression): Boolean = e match {
        case Conjunction(a, b) => eval(a) && eval(b)
        case Disjunction(a, b) => eval(a) || eval(b)
        case Negation(a) => !eval(a)
        case Value(v) => v
        case Variable(_) => throw new RuntimeException("Unable to evaluate variable without world")
    }

    def eval(e: Expression, world: Set[String]): Boolean = eval(assign(e, world))
}
