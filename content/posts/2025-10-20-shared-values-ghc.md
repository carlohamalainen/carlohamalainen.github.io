---
date: 2025-10-20
title: Shared values in GHC
url: /2025/10/20/shared-values-ghc
---

## The Question

When we have many data structures that contain the same large value, does GHC preserve sharing to save memory?

For example, if we create 100 `Foo` values that each contain the same large `HashSet`, will GHC be smart enough to share that single `HashSet` in memory, or will it create 100 separate copies?

This minimal example demonstrates that GHC does preserve sharing when we explicitly share a value at the top level.

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

## Testing the Difference

Now in `cabal repl`:

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

## The Results

The difference is dramatic:

- **Shared version**: 1.2 GB allocated, 0.53 seconds
- **Unshared version**: 117 GB allocated, 37.25 seconds

When we share the `HashSet` via the top-level `sharedHashSet` binding, GHC preserves this sharing. Each of the 100 `Foo` values points to the same `HashSet` in memory, so we only allocate it once.

In contrast, when we create a fresh `HashSet` for each `Foo` value in `makeUnsharedFoos`, we allocate 100 separate copies of the same million-element set, leading to roughly **100x more memory allocation** and much slower execution.

This demonstrates that GHC respects explicit sharing: when you bind a value and reuse it, the runtime will share that value in memory rather than duplicating it. Using a top-level CAF (Constant Applicative Form) like `sharedHashSet` ensures the value is shared across the entire program.
