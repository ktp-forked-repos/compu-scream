# Constraint propagation on computational logic

This is an experimental framework for expressing and enforcing
constraints on computational logic. It is also an excursion into the
world of nondeterministic programming.

The DSL(-ish environment) is based on [Screamer] which is in turn
based on Common Lisp. I run this on [SBCL].

Each logical bit is represented as a boolean Screamer variable. On top
of this, a bunch of macros allow succinct expression of constraints
for complete groups of multi-bit integers connected via complex
computation logic.

Via constraint propagation, computing the inverse of an invertable
function (computing "backwards", finding the input based on the output
given) is just as feasible as the regular evaluation.

The concept is sufficiently advanced that the SHA256 block computation
can be expressed and performed. (Note: you won't be able to break
Bitcoin by using this -- or other -- constraint solvers, this is
purely chosen for entertainment value only. You have been warned.)

Look at `*main.lisp` for entry points. In particular, loading `main.lisp`
will run all the unit tests.

All is completely undocumented and highly experimental.


[Screamer]:  https://nikodemus.github.io/screamer/
[SBCL]:      http://www.sbcl.org/
