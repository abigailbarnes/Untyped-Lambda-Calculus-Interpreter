# Untyped-Lambda-Calculus-Interpreter
For HW4, you will complete an untyped lambda calculus ("ULC") interpreter.

Certain syntactic bonus forms are part of the design of this interpreter in order to make writing ULC programs a little less tedious. The kind of syntactic conveniences included here are commonly known as "syntactic sugar" (since they sweeten your programming experience).

The concrete syntax of this week's language is as follows:

Variables are one or more lowercase letters in any combination except keywords tru and fls.

ex: a, b, x, y, aaa, bbb, xyzw, etc.

Abstractions don't include a syntactic lambda, but are enclosed in square brackets, and have one or more variable names followed by a dot.

ex: [x . x], [x . y], [a . [x . x]], [a x . a], [b f s . ((b f) s)]

Note that [x y . x] is just shorthand for [x . [y . x]]. (Corrected: this used to say [x [y . x]], i.e., was missing a dot.) All ULC abstractions have exactly one variable.

Note whitespace around certain of these tokens is unnecessary; that is, you can write

[x.x] or [y.y] or [x.[y.y]]

if the lack of whitespace doesn't bother you.

Applications are always exactly two terms long and are enclosed in parentheses.

ex: (a b), ((a b) c), (a (b c)), ([x . x] [y . y])

You may write "tru" and "fls" instead of spelling out the standard Church booleans.

You may write natural numbers 0, 1, 2, ... instead of spelling out Church numerals.

You may write "identity function abbreviations" with one or more uppercase letters.

ex: A, B, (A A), (A B), [x . A]

Any identity function abbreviation is a stand-in for the identity function with that variable name.

ex: A means [A.A], B means [B.B], XYZ means [XYZ . XYZ], etc.

You will find it is useful to by able to type identities in this shorthand, especially during testing CBV evaluation.

Copy and paste the framework below (at the bottom of this post) into your repository as hw4/HW4.scala. The automatic cases of scanning and parsing are included in the framework, while the more interesting cases in both scanning and parsing are left for you to complete.

If you run $ grep -n todo HW4.scala you will see your list of tasks, with line numbers:

52:  case c::tl=> throw new Exception("todo: scan variable names, ID names, and numbers, and identify scan errors")
71:  case TLParen::tl => throw new Exception("todo: parse app")
72:  case TLBrack::TVar(x)::tl => throw new Exception("todo: parse abs")
84:def rewrite(t:SurfaceTerm): ULCTerm = throw new Exception("todo: rewrite a surface term to ULC term")
88:def fv(t:ULCTerm): Set[String] = throw new Exception("todo: fv (compute set of free variables)")
97:def subst(x: String, s: ULCTerm, t1: ULCTerm): ULCTerm = throw new Exception("todo: subst")
99:def stepCBV(t: ULCTerm): Option[ULCTerm] = throw new Exception("todo: stepCBV")
101:def stepLazy(t: ULCTerm): Option[ULCTerm] = throw new Exception("todo: stepLazy")
103:def stepBeta(t: ULCTerm): Option[ULCTerm] = throw new Exception("todo: stepBeta")
106:def unparse(t: ULCTerm): String = throw new Exception("todo: unparse ULC terms")

Thus, along with completing the scanner and parser, you need to do the following:

implement "rewrite" to desugar a SurfaceTerm into an ULCTerm,

implement "fv", an algorithm needed in substitution,

implement "subst", the one and only way ULC terms ever take any evaluation steps,

implement "stepCBV" for call-by-value ULC evaluation,

implement "stepLazy" for lazy ULC evaluation,

implement "stepBeta" for full-beta ULC evaluation, and

implement "unparse" to consume a parsed ULC term and turn it back into source code.

A few notes about each of these tasks:

rewrite: Think about how to rewrite each kind of surface term. The types of SurfaceTerm and ULCTerm will support you through this task.

fv: The simple FV algorithm is given is 5.3.2 in the text. Scala has good support for sets. Notably, it supports ++ and - for set union and set removal, respectively. For example:

scala> Set(1) 
val res0: Set[Int] = Set(1)

scala> Set(1,1,1) 
val res1: Set[Int] = Set(1)

scala> Set(1,2,2) 
val res2: Set[Int] = Set(1, 2)

scala> Set(1,2) ++ Set(1,3) 
val res3: Set[Int] = Set(1, 2, 3)

scala> Set(1,2,3) - 2 
val res4: Set[Int] = Set(1, 3)

subst: This is a little more involved; see below.

stepCBV: The rules for CBV evaluation appear in the text in Figure 5-3.

stepLazy: The rules for lazy evaluation appear in Appendix A under 5.3.6.

stepBeta: The rules for full-beta reduction are given in Appendix A under 5.3.6 but are missing, by mistake, the congruence rule for the body of an abstraction.

unparse does what it says it does. Note that every ULC term can be written in the very same concrete syntax as a surface term, so go ahead and use the surface concrete syntax. Since unparsing happens (in this implementation) after rewriting, it will contain no sugary terms, just pure ULC.

subst: We have the following algorithmic definition of substitution, developed in section 5.3 in the text.

[x |-> s] x = s // where the two xs are syntactically identical

[x |-> s] y = y // where x and y are syntactically distinct

[x |-> s] (t1 t2) = ([x |-> s]t1 [x |->s]t2)

[x |-> s] (lam x . t1) = lam x . t1 // where the two xs are syntactically identical

[x |-> s] (lam y . t1) = // where x and y are syntactically distinct

  if y is in FV(s) : [x |->s] (lam y' . t1') where y' is a fresh variable name and in t1' y is rewritten to y'

  if y is not in FV(x) : lam y . [x |-> s] t1

The supplied function freshVarName generates a new fresh variable name every time it is called. Read the code closely to understand how it does this (note that it cheats the scanner, syntactically). You'll need to call freshVarName in the y' case in implementing subst above.

And that's it! Once you have implemented the ULC, you could, in principle, use it as your only programming language hereafter, although, of course, you certainly won't.

The finished work is due on Thursday Oct 27 before the stroke of midnight.
