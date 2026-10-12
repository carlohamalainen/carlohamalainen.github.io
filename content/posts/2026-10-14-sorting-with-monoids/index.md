---
date: 2026-10-14
title: Sorting with Monoids
url: /2026/10/14/sorting-with-monoids
---

After [George Wilson's talk on Monoids](https://luma.com/bfpg?e=evt-wA3LBLRglEB8jHj)
I mentioned this [18 year old reddit post](https://www.reddit.com/r/programming/comments/7cf4r/comment/c06adnx/) 
about using Monoids for sorting; a [BFPG](https://bfpg.org) friend
requested a modern writeup given that `Monoid` split into `Semigroup` and
`Monoid` in GHC 8.4.

The trick hasn't changed, but the classes have, so let's start from the current definitions and work
towards our goal of sorting on arbitrary attributes of a type.

```haskell
class Semigroup a where
  (<>) :: a -> a -> a    -- must be associative!

class Semigroup a => Monoid a where
  mempty  :: a              -- identity for (<>)
  mappend :: a -> a -> a    -- legacy, should equal (<>)

  mconcat :: [a] -> a -- fold a list with <>, starting from mempty
```

`Semigroup` is for things that can be combined with an associative operation, i.e.

```haskell
a <> (b <> c) == (a <> b) <> c -- for all a, b, c
```

For `String`, the `Semigroup` instance is normal concatenation:

```haskell
instance Semigroup String where
  x <> y = x ++ y
```

With the empty string `""` we can define a `Monoid` instance:

```haskell
instance Monoid String where
  mempty = "" -- mempty <> x = x, x <> mempty = x for all x
```

We shouldn't let this colour our thinking: there are many values in the 
type `String`, but we can define `Semigroup` and `Monoid` instances on a type with
just one value, the unit type `()`.

```haskell
instance Semigroup () where
  () <> () = ()

instance Monoid () where
  mempty = ()
```

What about a type with two values? The integers under addition modulo 2:

```haskell
data Z2 = N0 | N1

instance Semigroup Z2 where
  N0 <> N0 = N0
  N0 <> N1 = N1
  N1 <> N0 = N1
  N1 <> N1 = N0

instance Monoid Z2 where
  mempty = N0
```

What about a type with three values? We could do integers under addition modulo 3, but instead let's look 
at the `Ordering` type for ordering:

```haskell
data Ordering = LT | EQ | GT

instance Semigroup Ordering where
  -- LT wins
  LT <> EQ = LT
  LT <> LT = LT
  LT <> GT = LT

  -- GT wins
  GT <> EQ = GT
  GT <> LT = GT
  GT <> GT = GT

  -- EQ is a tie, so use the other value
  EQ <> EQ = EQ
  EQ <> LT = LT
  EQ <> GT = GT

instance Monoid Ordering where
  mempty = EQ
```

Does this make sense? Let's check associativity, brute force:

```haskell
-- a = LT: LT on the left wins straight away, so both sides are LT.
(LT <> LT) <> LT == LT <> (LT <> LT)   -- LT <> LT == LT <> LT   -- LT == LT
(LT <> LT) <> EQ == LT <> (LT <> EQ)   -- LT <> EQ == LT <> LT   -- LT == LT
(LT <> LT) <> GT == LT <> (LT <> GT)   -- LT <> GT == LT <> LT   -- LT == LT
(LT <> EQ) <> LT == LT <> (EQ <> LT)   -- LT <> LT == LT <> LT   -- LT == LT
(LT <> EQ) <> EQ == LT <> (EQ <> EQ)   -- LT <> EQ == LT <> EQ   -- LT == LT
(LT <> EQ) <> GT == LT <> (EQ <> GT)   -- LT <> GT == LT <> GT   -- LT == LT
(LT <> GT) <> LT == LT <> (GT <> LT)   -- LT <> LT == LT <> GT   -- LT == LT
(LT <> GT) <> EQ == LT <> (GT <> EQ)   -- LT <> EQ == LT <> GT   -- LT == LT
(LT <> GT) <> GT == LT <> (GT <> GT)   -- LT <> GT == LT <> GT   -- LT == LT

-- a = EQ: EQ defers to whatever b <> c decides.
(EQ <> LT) <> LT == EQ <> (LT <> LT)   -- LT <> LT == EQ <> LT   -- LT == LT
(EQ <> LT) <> EQ == EQ <> (LT <> EQ)   -- LT <> EQ == EQ <> LT   -- LT == LT
(EQ <> LT) <> GT == EQ <> (LT <> GT)   -- LT <> GT == EQ <> LT   -- LT == LT
(EQ <> EQ) <> LT == EQ <> (EQ <> LT)   -- EQ <> LT == EQ <> LT   -- LT == LT
(EQ <> EQ) <> EQ == EQ <> (EQ <> EQ)   -- EQ <> EQ == EQ <> EQ   -- EQ == EQ
(EQ <> EQ) <> GT == EQ <> (EQ <> GT)   -- EQ <> GT == EQ <> GT   -- GT == GT
(EQ <> GT) <> LT == EQ <> (GT <> LT)   -- GT <> LT == EQ <> GT   -- GT == GT
(EQ <> GT) <> EQ == EQ <> (GT <> EQ)   -- GT <> EQ == EQ <> GT   -- GT == GT
(EQ <> GT) <> GT == EQ <> (GT <> GT)   -- GT <> GT == EQ <> GT   -- GT == GT

-- a = GT: GT on the left wins straight away, so both sides are GT.
(GT <> LT) <> LT == GT <> (LT <> LT)   -- GT <> LT == GT <> LT   -- GT == GT
(GT <> LT) <> EQ == GT <> (LT <> EQ)   -- GT <> EQ == GT <> LT   -- GT == GT
(GT <> LT) <> GT == GT <> (LT <> GT)   -- GT <> GT == GT <> LT   -- GT == GT
(GT <> EQ) <> LT == GT <> (EQ <> LT)   -- GT <> LT == GT <> LT   -- GT == GT
(GT <> EQ) <> EQ == GT <> (EQ <> EQ)   -- GT <> EQ == GT <> EQ   -- GT == GT
(GT <> EQ) <> GT == GT <> (EQ <> GT)   -- GT <> GT == GT <> GT   -- GT == GT
(GT <> GT) <> LT == GT <> (GT <> LT)   -- GT <> LT == GT <> GT   -- GT == GT
(GT <> GT) <> EQ == GT <> (GT <> EQ)   -- GT <> EQ == GT <> GT   -- GT == GT
(GT <> GT) <> GT == GT <> (GT <> GT)   -- GT <> GT == GT <> GT   -- GT == GT
```

All 27 cases agree, so `<>` on `Ordering` really is associative. The first non-`EQ` value decides the 
result, and that is true no matter where the parentheses go.

Similarly, for `Monoid`:

```haskell
-- Left identity: mempty <> x == x, with mempty = EQ.
mempty <> LT == LT   -- EQ <> LT == LT   -- LT == LT
mempty <> EQ == EQ   -- EQ <> EQ == EQ   -- EQ == EQ
mempty <> GT == GT   -- EQ <> GT == GT   -- GT == GT

-- Right identity: x <> mempty == x.
LT <> mempty == LT   -- LT <> EQ == LT   -- LT == LT
EQ <> mempty == EQ   -- EQ <> EQ == EQ   -- EQ == EQ
GT <> mempty == GT   -- GT <> EQ == GT   -- GT == GT
```

All 6 cases agree, so `EQ` is an identity for `<>` on both sides, and `Ordering` is a
`Monoid`. Checking both sides matters here because `<>` on `Ordering` is not
commutative: `LT <> GT` is `LT` but `GT <> LT` is `GT`.

## Sorting people

Suppose we have a record type for a person:

```haskell
data Person = Person 
    { name      :: String
    , age       :: Int
    , country   :: String
    }
```

We are asked to sort by name, then age, then country (basically a dictionary style sorting).

Here's our first attempt, fully manual:

```haskell
byNameThenAgeThenCountry :: Person -> Person -> Ordering
byNameThenAgeThenCountry (Person n0 a0 c0) (Person n1 a1 c1)
    -- 1. Compare names
    | n0 < n1
    = LT

    | n0 > n1
    = GT

    -- 2. Names tie: compare ages
    | n0 == n1
    , a0 < a1
    = LT

    | n0 == n1
    , a0 > a1
    = GT

    -- 3. Names and ages tie: compare countries
    | n0 == n1
    , a0 == a1
    , c0 < c1
    = LT

    | n0 == n1
    , a0 == a1
    , c0 > c1
    = GT

    -- 4. Everything ties
    | otherwise
    = EQ
```

This is agony. Too many cases, too easy to get things wrong, and it doesn't
scale to more comparison fields. But it does work with the standard library
`sortBy` function:

```haskell
import Data.List ( sortBy )

sortBy :: (a -> a -> Ordering) -> [a] -> [a]

sortByNameThenAgeThenCountry :: [Person] -> [Person]
sortByNameThenAgeThenCountry = sortBy byNameThenAgeThenCountry 
```

Wouldn't it be nicer if we could "lift" a function `Person -> u` to a comparison function
`Person -> Person -> Ordering`? And then combine them with `<>`? How would this work?

By definition, the records give us functions

```haskell
name      :: Person -> String
age       :: Person -> Int
country   :: Person -> String
```

So we need something like

```haskell
lift :: (Person -> u) -> Person -> Person -> Ordering
```

which would let us write

```haskell
sortBy (lift name)
sortBy (lift age)
sortBy (lift country)
```

So this `lift` function must be

```haskell
lift :: (t -> u) -> t -> t -> Ordering -- won't compile yet...
lift f x y
  | f x < f y = LT
  | f x > f y = GT
  | otherwise = EQ
```

We have actually discovered `comparing` from `Data.Ord` (and realise that we need to be able to compare the values, hence an `Ord` constraint!).

```haskell
comparing :: Ord a => (b -> a) -> b -> b -> Ordering
```

So where are we? We can write

```haskell
comparing name      :: Person -> Person -> Ordering
comparing age       :: Person -> Person -> Ordering
comparing country   :: Person -> Person -> Ordering
```

and we can use them with `sortBy`:

```haskell
sortBy (comparing name)      :: [Person] -> [Person]
sortBy (comparing age)       :: [Person] -> [Person]
sortBy (comparing country)   :: [Person] -> [Person]
```

All good so far!

Returning to our original problem, wouldn't it be nice if we could combine the `comparing f` forms?

```haskell
nameAgeCountry = comparing name ~:~ comparing age ~:~ comparing country
```

and use this as `sortBy nameAgeCountry`? Then we can do different orderings like 

```haskell
sortBy (comparing name ~:~ comparing age)
sortBy (comparing name ~:~ comparing country ~:~ comparing age)
sortBy (comparing age  ~:~ comparing country ~:~ comparing name)
```

What's the type of this magical combinator `~:~`? It must be

```haskell
(~:~) :: (Person -> Person -> Ordering)
      -> (Person -> Person -> Ordering)
      -> (Person -> Person -> Ordering)
```

Squint at that type. Writing `Cmp` for `Person -> Person -> Ordering`, it's

```haskell
type Cmp = Person -> Person -> Ordering

(~:~) :: Cmp -> Cmp -> Cmp
```

which is exactly the shape of `(<>) :: a -> a -> a` with `a = Cmp`. So what
we're really asking for is a `Semigroup` instance for the function type
`Person -> Person -> Ordering`.

Let's try to write it. We're handed two comparison functions `f` and `g` and
two people `x` and `y`. The only sensible thing to do is run both comparisons:

```haskell
f x y :: Ordering
g x y :: Ordering
```

and then combine the two results: if `f` has an opinion, go with it, otherwise
fall back to `g`. "First non-`EQ` wins" is precisely the `<>` on `Ordering`
from the start of this post. So:

```haskell
(f ~:~ g) x y = f x y <> g x y
```

That's the whole combinator. 

Notice that it does not use anything about `Person`. And it doesn't even care about `Ordering`, except that it has a `<>` operation. So we can generalise: any function into a `Semigroup` is itself a
`Semigroup`, combined pointwise:

```haskell
instance Semigroup b => Semigroup (a -> b) where
  (f <> g) x = f x <> g x
```

Read `Person -> Person -> Ordering` as `Person -> (Person -> Ordering)` and
apply the instance twice: `Ordering` is a `Semigroup`, so `Person -> Ordering`
is a `Semigroup`, so `Person -> Person -> Ordering` is a `Semigroup`.
Unfolding:

```haskell
(f <> g) x y = (f x <> g x) y = f x y <> g x y
```

which is exactly `~:~`.

Is it associative? Pick any `x` and `y` and unfold both sides:

```haskell
((f <> g) <> h)  x y = (f <> g) x y <> h x y = (f x y <>  g x y) <> h x y
(f  <> (g <> h)) x y = f x y <> (g <> h) x y =  f x y <> (g x y  <> h x y)
```

The right-hand sides are three `Ordering` values bracketed in two different
ways, and we manually checked the 27 cases earlier. Associativity of
functions-into-`Ordering` is inherited from associativity of `Ordering`.

The same trick gives `mempty`:

```haskell
instance Monoid b => Monoid (a -> b) where
  mempty = \_ -> mempty
```

On our type, this is `mempty = \_ _ -> EQ`, the comparison that thinks everybody
ties, and it's an identity for the same reason:

```haskell
(mempty <> f) x y = EQ <> f x y = f x y
(f <> mempty) x y = f x y <> EQ = f x y
```

So our combinator `~:~` is actually `<>` from `Semigroup`!
The agonising function at the top of this section becomes

```haskell
import Data.List ( sortBy )
import Data.Ord  ( comparing )

sortByNameThenAgeThenCountry :: [Person] -> [Person]
sortByNameThenAgeThenCountry = sortBy (comparing name <> comparing age <> comparing country)
```

The reader might say, eh, why not just map to tuples? Indeed we could:

```haskell
sortOn (\p -> (name p, age p, country p))
```

using the other function `sortOn` in `Data.List`.

However the "encoding" of the sorting decision is now hidden inside a lambda expression.

In contrast, our comparators are ordinary values. We can put them in a list and
fold them together with `mconcat`:

```haskell
import Data.Ord ( comparing, Down(..) )

nameAgeCountry :: [Person -> Person -> Ordering]
nameAgeCountry = [comparing name, comparing (Down . age), comparing country]

sortByNameThenAgeThenCountry :: [Person] -> [Person]
sortByNameThenAgeThenCountry = sortBy (mconcat nameAgeCountry)
```

Reordering the sort keys is now reordering a list. `Down` flips the direction
of a single key, so this sorts by name, then oldest first, then country. And
because the list is just data, we can build it at runtime, say from the columns
a user clicked on in a table. The empty list of keys is `mconcat []`, which is
`mempty`.

And maybe the coolest sort function of them all, `sortBy mempty` compares
nothing, so it leaves the list exactly as it found it.
