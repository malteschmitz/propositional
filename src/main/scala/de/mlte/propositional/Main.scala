package de.mlte.propositional

import Expression._

@main def main = {
    val f = Negation(Conjunction(Negation(Variable("a")), Variable("b")))
    println(print(f))
    println(print(nnf(f)))
    println(eval(f, Set("a")))
    println(eval(f, Set("b")))
}