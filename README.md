# hs-z3

Haskell bindings to the [Z3 SMT solver](https://github.com/Z3Prover/z3).
These bindings are primarily to support my own use cases where I found the
existing [z3](https://hackage.haskell.org/package/z3) package to be lacking,
but I'm open to adding more features if there is interest. In general though
the existing bindings include more utility and are considerably more mature
than these.

This package also includes a higher-level module which mirrors (a subset of)
the Z3 expression structures in pure Haskell code. This allows users to
interact with AST's in a purely functional way and only drop into IO when you
need to interact with Z3.
