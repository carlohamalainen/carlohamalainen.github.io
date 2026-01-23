---
date: 2025-10-20
title: Shared values in GHC
url: /2025/10/20/shared-values-ghc
---

Standard procedural languages like C and Go let the programmer reason about
memory sharing by exposing raw pointer values. In C everything is explicit
so you know what's happening. In Go, despite the garbage collector, one can
still check for pointer equality:


```go
func main() {
    x := "foo"
    y := "bar"
    fmt.Println(&x == &y) // false
}
```

Haskell's runtime system with lazy evaluation makes this notoriously difficult.
Partially evaluated expressions (thunks) are reduced until the program produces
its final output. The graph is invisible, and non-expert developers have very
little intuition about what is going on behind the scenes.

We can do some basic experiments at the repl to check how much sharing is
occuring between shared values in a data structure.

For example, if we create 100 `Foo` values that each contain the same large
`HashSet`, will GHC keep just one copy of the large `HashSet` in memory, or
will it create 100 separate copies?

## The Code

```haskell
{-# LANGUAGE BangPatterns #-}

module SharedValues where

import Data.HashSet (HashSet)
import qualified Data.HashSet as HS

data Foo = Foo Int (HashSet Int)
  deriving (Show)

makeLargeHashSet :: Int -> HashSet Int
makeLargeHashSet n = HS.fromList [1..n]

sharedHashSet :: HashSet Int
sharedHashSet = makeLargeHashSet 1000000

makeSharedFoos :: Int -> [Foo]
makeSharedFoos count = map (`Foo` sharedHashSet) [1..count]

makeUnsharedFoos :: Int -> [Foo]
makeUnsharedFoos count = map (\i -> Foo i (makeLargeHashSet 1000000)) [1..count]

forceFoos :: [Foo] -> ()
forceFoos [] = ()
forceFoos (Foo !_ !_ : rest) = forceFoos rest

testShared :: Int -> [Foo]
testShared count =
  let !foos = makeSharedFoos count
      !_ = forceFoos foos
  in foos

testUnshared :: Int -> [Foo]
testUnshared count =
  let !foos = makeUnsharedFoos count
      !_ = forceFoos foos
  in foos

```

Now in `cabal repl`, use `:set +s` to get memory statistics:

```
ghci> :set +s
ghci> import SharedValues
(0.00 secs, 0 bytes)

ghci> {-# LANGUAGE BangPatterns #-}
(0.00 secs, 0 bytes)

ghci> let !x = testShared 100 in ()
()
(0.53 secs, 1,169,687,984 bytes)

ghci> let !x = testUnshared 100 in ()
()
(37.25 secs, 116,909,472,272 bytes)
```

We use bang patterns `!x` to force all values.

The difference is dramatic:

- **Shared version**: 1.2 GB allocated, 0.53 seconds
- **Unshared version**: 117 GB allocated, 37.25 seconds

When we share the `HashSet` via the top-level `sharedHashSet` binding, GHC
preserves this sharing. Each of the 100 `Foo` values points to the same
`HashSet` in memory, so we only allocate it once.

In contrast, when we create a fresh `HashSet` for each `Foo` value in
`makeUnsharedFoos`, we allocate 100 separate copies of the same million-element
set, leading to roughly 100x more memory allocation. Memory allocation is
expensive, reflecting the huge impact on runtime (0.53 seconds vs 37 seconds).

This demonstrates that GHC respects explicit sharing: when you bind a value and
reuse it, the runtime will share that value in memory. 

