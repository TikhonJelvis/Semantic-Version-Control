See https://docs.google.com/document/pub?id=1rCgag9aDUA1W8T9pfZDeClnEDwxvJm51Dwh-KHBkYsM

# Cow - The Semantic Version Control System!

Normal version control systems operate on text which just happens to
make up programs. However, programs are not fundamentally made up of
_text_—they are made up of _code_. Our version control system takes
this into account, versioning not the actual text of the program but a
parse tree.

Operating on the semantic meaning of the code rather than its text
offers many advantages for all facets of version control. Using this
system, we have more meaningful diffs and larger amounts of
information about the history of the. For example, we know when a revision
has only changed comments; these revisions can be ignored when looking
for bugs because they did not affect that actual code of the program
at all.

Additionally, the semantic information can be used to improve both
diffing and merging. We know when two variables are the same—not only
in terms of their code, but in terms of the meaning of the code. We
can ignore variables that are in different scopes but only share a
name while unifying variables that are actually the same across
disparate parts of the source code.

## Technology

This was a complicted problem that we split up into three different
parts. Each of us executed one of these parts. Each part contained
its own non-trivial problems. Additionally, we each used a different
language for each part:

  - The front-end uses JavaScript and HTML/CSS. The nontrivial problem
    here was pretty-printing the code.
  - The server uses Scala and the Lift framework. The nontrivial
    problem here was the actual tree diff algorithm.
  - The command-line front end and parser use Haskell and the Parsec
    parser-combinator library. The nontrivial problem here was
    producing a parse tree along with some static-analysis of the
    code.

In the end, all three of these parts come together into a coherent
whole, a minimal but functional semantic version control system for
Scheme.
