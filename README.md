Pup
===

_Who's a good format descriptor?_

Pup is a library to define format descriptors which can be interpreted both as a
parser and as a pretty-printer. This obviates the common need to maintain separate
parsers and pretty-printers.

The Pup library is focused on ergonomics, the
goal is for grammars to be barely noisier than, say, a Megaparsec parser.

For instance for a type

``` haskell
data T = C Int Bool | D Char Bool Int
```

A Megaparsec parser could look like

``` haskell
C <$> chunk "C" *> space1 *> int <* space1 <*> bool
<|> D <$> chunk "D" *> space1 *> anySingle <* space1 <*> bool <* space1 <*> int
```

A Pup format descriptor for the same type could look like

``` haskell
#C <* chunk "C" <* space1 <*> int <* space1 <*> bool
<|> #D <* chunk "D" <* space1 <*> anySingle <* space1 <*> bool <* space1 <*> int
```

But the Pup descriptor can pretty-print in addition to parse.

## Some underlying principles

Our article _Invertible Syntax without the Tuples_ (published version (TBA),
[extended version with appendices][paper-extended]) goes over a lot of the
design decisions which went into the Pup library, in particular how it compares
with previous approaches.

Here are some highlights:

- Pup uses indexed monads, the indices represent a stack in continuation-passing
  form. For instance a format descriptor for natural numbers has type
  ```haskell
  nat :: … => m (Int -> r) r Int
  ```
  We read `m (Int -> r) r` as meaning that the printer for `nat` reads (and
  pops) an `Int` off the stack; the final `Int` means that the parser for `nat`
  returns an `Int` (after consuming some input).
- Pup doesn't implement its own parsers and printers. Instead format descriptors
  rely on backends. Currently, Pup provides a [Megaparsec] backend for parsing,
  and a [Prettyprinter] backend for printing.
- Pup's type-class based interface makes extensible. You can swap backends for
  parsers and printers independently. Pup even supports, at least in principle,
  format descriptors which can be interpreted in several parser or printer
  backends, should that make sense.

## Why the name?

Pup ostensibly stands for _Parser-UnParser_ (“unparsing” is a term coined by
Danvy in his [_Functional unparsing_][functional-unparsing-paper], to which this
library is indebted; the definition of “unparsing” is nebulous but it can be
understood a pretty-printing).

But this library is also meant to build upon the prior work of, and be a more
practical version of, the [Cassette] library. Cassette is sometimes stylised as
K7 (after the French pronunciation of both words). And pups are K9.

Now you know.

[functional-unparsing-paper]: https://www.cambridge.org/core/journals/journal-of-functional-programming/article/functional-unparsing/789945109AD2AB168B504472E6B786A0
[Cassette]: https://github.com/mboes/cassette/
[paper-extended]: https://arxiv.org/abs/2508.09856
[Megaparsec]: https://hackage.haskell.org/package/megaparsec
[Prettyprinter]: https://hackage.haskell.org/package/prettyprinter
