---
author: Carlo Hamalainen
date: "2021-07-17T00:00:00Z"
title: Superset trie search
url: /2021/07/17/superset-trie
---

A normal map lets us look up items based on an exact match for the key,
but here is a situation where we want to match on a _superset_ of the
search key. We generate some expensive objects related to sets of tags. For example:

    { "foo", "bar", "baz" } => #{expensive object 0}
    { "apple", "cat" }      => #{expensive object 1}
    { "dog" }               => #{expensive object 2}

Example lookups:

    { "foo", "bar", "baz" } => #{expensive object 0}
    { "bar", "baz" }        => #{expensive object 0}
    { "apple }              => #{expensive object 1}
    { "foo", "dog" }        => none

The simplest way to implement this is to iterate through a
``[([String], ExpensiveObject)]`` and check for list containment. This is very slow.

A better idea is to iterate [isSubsetOf](https://hackage.haskell.org/package/containers-0.6.5.1/docs/Data-Set.html#v:isSubsetOf) over a list of sets. This can be fairly quick because ``Data.Set`` is implemented using balanced binary
trees and simple heuristics remove expensive recursive calls in ``isSubsetOf``:

```haskell

-- https://hackage.haskell.org/package/containers-0.6.5.1/docs/src/Data.Set.Internal.html#isSubsetOf

isSubsetOfX :: Ord a => Set a -> Set a -> Bool
isSubsetOfX Tip _ = True
isSubsetOfX _ Tip = False
-- Skip the final split when we hit a singleton.
isSubsetOfX (Bin 1 x _ _) t = member x t
isSubsetOfX (Bin _ x l r) t
  = found &&
    -- Cheap size checks can sometimes save expensive recursive calls when the
    -- result will be False. Suppose we check whether [1..10] (with root 4) is
    -- a subset of [0..9]. After the first split, we have to check if [1..3] is
    -- a subset of [0..3] and if [5..10] is a subset of [5..9]. But we can bail
    -- immediately because size [5..10] > size [5..9].
```

Another idea is to augment a [trie](https://en.wikipedia.org/wiki/Trie) to allow for key-superset search.
The paper [Index Data Structure for Fast Subset and Superset Queries (Savnik)](https://github.com/carlohamalainen/superset-trie/blob/main/978-3-642-40511-2_10_Chapter.pdf) shows how to do this.

Following the paper we can implement a superset trie like so:

```haskell 
data STrie a b = STrie (Maybe b) (M.Map a (STrie a b))
```

The ``Maybe b`` is a place to store an ``ExpensiveObject``, and the
``Map`` gives the children of this node.

Inserting into the ``STrie`` is essentially the same as in a normal trie but we 
first sort the elements of the set. Searching also takes advantage of the sorted search key:

```haskell
member :: (Ord a) => [a] -> STrie a b -> Bool
member [] _ = False
member k strie = member' (sortNub k) strie
  where
    member' [] (STrie _ _) = True

    member' (x:xs) (STrie _ nodes) = with || without
      where

        -- (1) Use x, which means finding a child trie labeled x.
        with = Just True == (member' xs <$> M.lookup x nodes)

        -- (2) Skip x, which means finding some child tree where
        -- the recursive call works. Since x:xs is sorted and the
        -- values inserted into the trie are sorted, we don't
        -- need to look at values where x is >= the label.
        --
        -- For example if x = "foo" and the children are
        --
        -- "bar", "egg", "zed"
        -- 
        -- then there's no point following the "zed" sub-trie
        -- since we will never find a "foo" there, due
        -- to the fact that "zed" > "foo".
        nodes' = M.filterWithKey (\k _ -> k < x) nodes

        without = any (member' (x:xs) . snd) $ M.toList nodes'
```

Here are some benchmarks. I used [criterion](https://hackage.haskell.org/package/criterion) and re-used
the generators I defined for the [QuickCheck](https://hackage.haskell.org/package/QuickCheck) tests.

```
benchmarking subset-find/subset-trie
time                 495.7 ms   (476.1 ms .. 537.9 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 481.1 ms   (477.2 ms .. 488.8 ms)
std dev              7.641 ms   (94.67 μs .. 9.084 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking subset-find/Data.Set
time                 2.606 s    (2.127 s .. 3.647 s)
                     0.981 R²   (0.967 R² .. 1.000 R²)
mean                 2.687 s    (2.476 s .. 2.860 s)
std dev              231.5 ms   (120.8 ms .. 314.1 ms)
variance introduced by outliers: 22% (moderately inflated)

benchmarking subset-find/naive-lists
time                 8.774 s    (6.096 s .. 9.930 s)
                     0.989 R²   (0.972 R² .. 1.000 R²)
mean                 10.22 s    (9.482 s .. 10.65 s)
std dev              724.3 ms   (294.8 ms .. 995.7 ms)
variance introduced by outliers: 20% (moderately inflated)
```

![benchmark](https://raw.githubusercontent.com/carlohamalainen/superset-trie/main/subset-trie-benchmarks.png)

Full source: [https://github.com/carlohamalainen/superset-trie](https://github.com/carlohamalainen/superset-trie)
