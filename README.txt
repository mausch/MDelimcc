This is a port to F# of the delimited continuation monad in OCaml by Kiselyov et al.
Original code: http://www.cas.mcmaster.ca/~carette/pa_monad/cc.ml
As this is derivative work, it's licensed under the Library GPL, see http://www.cas.mcmaster.ca/~carette/pa_monad/COPYING

Basically, it's the standard continuation monad with additional functions to implement multi-prompt delimited continuations (shift/reset).

The current implementation is not thread-safe.

See the tests for examples.