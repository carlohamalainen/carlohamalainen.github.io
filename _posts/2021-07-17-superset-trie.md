---
title: Superset trie search
date: 2021-07-17T00:00:00+00:00
author: Carlo Hamalainen
layout: post
permalink: /2021/07-17/superset-trie
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
``[ ([String], ExpensiveObject) ]`` and check for list containment. This is very slow.

A better idea is to iterate [isSubsetOf](https://hackage.haskell.org/package/containers-0.6.5.1/docs/Data-Set.html#v:isSubsetOf) over a list of sets. This can be fairly quick because ``Data.Set`` is implemented using balanced binary
trees and simple heuristics remove expensive recursive calls in ``isSubsetOf``:

{% highlight haskell %}

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
{% endhighlight %}

Another idea is to augment a [trie](https://en.wikipedia.org/wiki/Trie) to allow for key-superset search.
The paper [Index Data Structure for Fast Subset and Superset Queries (Savnik)](https://github.com/carlohamalainen/superset-trie/blob/main/978-3-642-40511-2_10_Chapter.pdf) shows how to do this.

Following the paper we can implement a superset trie like so:

{% highlight haskell %}
data STrie a b = STrie Bool (Maybe b) (M.Map a (STrie a b))
{% endhighlight %}

The ``Bool`` indicates if we are at the end of a value; the ``Maybe b`` is a place
to store an ``ExpensiveObject``, and the ``Map`` gives the children of this node.

Inserting into the ``STrie`` is essentially the same as in a normal trie but we 
first sort the elements of the set. Searching also takes advantage of the sorted search key:

{% highlight haskell %}
member :: (Ord a) => [a] -> STrie a b -> Bool
member = member' . List.nub . List.sort
  where
    member' [] (STrie end _ _) = end

    member' (x:xs) (STrie _ _ nodes) = with || without
      where

        -- (1) Use x, which means finding a child trie labelled x.
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
{% endhighlight %}

Here are some benchmarks. I used [criterion](https://hackage.haskell.org/package/criterion) and re-used
the generators I defined for the [QuickCheck](https://hackage.haskell.org/package/QuickCheck) tests.

```
benchmarking subset-find/subset-trie
time                 677.4 ms   (423.8 ms .. 851.0 ms)
                     0.980 R²   (0.965 R² .. 1.000 R²)
mean                 716.1 ms   (672.6 ms .. 758.8 ms)
std dev              49.40 ms   (41.35 ms .. 52.64 ms)
variance introduced by outliers: 20% (moderately inflated)

benchmarking subset-find/Data.Set
time                 2.485 s    (2.445 s .. 2.522 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 2.445 s    (2.417 s .. 2.460 s)
std dev              27.12 ms   (7.446 ms .. 36.28 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking subset-find/naive-lists
time                 9.857 s    (9.578 s .. 10.01 s)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 10.13 s    (10.01 s .. 10.35 s)
std dev              227.3 ms   (3.195 ms .. 270.8 ms)
variance introduced by outliers: 19% (moderately inflated)
```

![benchmark](https://raw.githubusercontent.com/carlohamalainen/superset-trie/main/subset-trie-benchmarks.png)

Full source: [https://github.com/carlohamalainen/superset-trie](https://github.com/carlohamalainen/superset-trie)
