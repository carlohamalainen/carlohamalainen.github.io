---
author: Carlo Hamalainen

date: "2015-06-30T00:00:00Z"
format: image
title: Lens Has/As for API changes
url: /2015/06/30/lens-hasas-for-api-changes/
---

Tinkering with lenses to deal with API changes.

Literate Haskell source for this post: [https://github.com/carlohamalainen/playground/tree/master/haskell/lens-has](https://github.com/carlohamalainen/playground/tree/master/haskell/lens-has).

First, some extensions and imports.

```haskell
{-# LANGUAGE GADTs                        #-}
{-# LANGUAGE FlexibleInstances            #-}
{-# LANGUAGE MultiParamTypeClasses        #-}
{-# LANGUAGE TemplateHaskell              #-}

module LensHas where

import Control.Applicative
import Control.Lens
import Numeric.Natural
```

## Introduction

Suppose we are working with a database service that stores files. Perhaps we communicate with it via a REST API. A file stored in the system has a location, which is a ``FilePath``:

```haskell
type Location = FilePath
```

We need to keep track of a few other things like the parent (referring to a collection of files) and a hash of the file. For simplicity I'll make those two fields ``String``s since the details aren't important to us here.

```haskell
data DataFile = DataFile {
    _dataFileLocation :: Location
  , _dataFileParent   :: String
  , _dataFileHash     :: String
} deriving Show
```

(Ignore the underscores if you haven't used lenses before.)

After some time the API changes and we need to keep track of some
different fields, so our data type changes to:

```haskell
data DataFile2 = DataFile2 {
    _dataFile2Location   :: Location
  , _dataFile2Parent     :: String
  , _dataFile2OtherField :: Float -- new field
                                  -- hash is not here anymore
} deriving Show
```

For compatibility we'd like to keep both definitions around, perhaps allowing the user
to choose the v1 or v2 API with a configuration option. So how do we deal with our code that
has to use ``DataFile`` or ``DataFile2``? One option is to use a sum type:

```haskell
data DataFileSum = DFS1 DataFile | DFS2 DataFile2
```

Any function that uses a ``DataFile`` must instead
use ``DataFileSum`` and do case analysis on whether it is
a v1 or v2.

In my particular situation I had a number of functions that used
just the ``Location`` part of the type. Is there a way to
avoid the sum type?

## Setter/Getter typeclasses

Use typeclasses to represent setting or getting the location value:

```haskell
class SetLocation a where
  setLocation :: a -> Location -> a

class GetLocation a where
  getLocation :: a -> Location
```

Write the instance definitions for each case:

```haskell
instance SetLocation DataFile where
  setLocation d newLocation = d { _dataFileLocation = newLocation }

instance GetLocation DataFile where
  getLocation = _dataFileLocation

instance SetLocation DataFile2 where
  setLocation d newLocation = d { _dataFile2Location = newLocation }

instance GetLocation DataFile2 where
  getLocation = _dataFile2Location
```

Now we use the general ``getLocation`` and ``setLocation`` functions instead of
the specific data constructors of ``DataFile`` and ``DataFile2``:

```haskell
main1 = do
  let df = DataFile "/foo/bar.txt" "something" "700321159acb26a5fd6d5ce0116a6215"

  putStrLn $ "Original data file: " ++ show df
  putStrLn $ "Location in original: " ++ getLocation df

  let df' = setLocation df "/blah/bar.txt"

  putStrLn $ "Updated data file:    " ++ getLocation df'
```

A function that uses a datafile can now be agnostic about which one it is, as long as the typeclass
constraint is satisfied so that it has the appropriate getter/setter:

```haskell
doSomething :: GetLocation a => a -> IO ()
doSomething d = print $ getLocation d
```

Using ``doSomething``:

```
*LensHas> doSomething $ DataFile "/foo/bar.txt" "parent" "12345"
"/foo/bar.txt"

*LensHas> doSomething $ DataFile2 "/foo/bar.txt" "parent" 42.2
"/foo/bar.txt"
```

## Lenses

Lenses already deal with the concept of getters and setters, so let's try to replicate the previous
code in that framework.

First, make lenses for the two data types (this uses Template Haskell):

```haskell
makeLenses ''DataFile
makeLenses ''DataFile2
```

Instead of type classes for setting and getting, make a single type class
that represents the fact that a thing _has_ a location.

```haskell
class HasLocation a where
    location :: Lens' a Location
```

For the instance definitions we can use the lenses that were automatically made for us by the earlier ``makeLenses`` lines:

```haskell
instance HasLocation DataFile where
    location = dataFileLocation :: Lens' DataFile Location

instance HasLocation DataFile2 where
    location = dataFile2Location :: Lens' DataFile2 Location
```

Here is ``main1`` rewritten to use the ``location`` lens:

```haskell
main2 = do
  let df = DataFile "/foo/bar.txt" "something" "700321159acb26a5fd6d5ce0116a6215"

  putStrLn $ "Original data file: " ++ show df
  putStrLn $ "Location in original: " ++ df^.location

  let df' = df & location .~ "/blah/bar.txt"

  putStrLn $ "Updated data file:    " ++ getLocation df'
```

If you haven't used lenses before the operators like ``^.`` might look insane, but there is a pattern to them. Check out [http://intolerable.me/lens-operators-intro](http://intolerable.me/lens-operators-intro/) for an excellent guide with examples.

One benefit of the lens approach is that we don't have to
manually write the setters and getters, as they come for free
from the lenses for the original two data types. Another benefit
is that lenses compose, so if the ``Location`` type was more than just a string,
we wouldn't have to manually deal with the composition of ``getLocation`` with ``getSubPartOfLocation`` and so on.

The ``doSomething`` function can be rewritten using the ``HasLocation`` typeclass:

```haskell
doSomething' :: HasLocation a => a -> IO ()
doSomething' d = print $ d^.location
```

## Generalising HasLocation

Let's generalise the ``HasLocation`` typeclass. Consider natural numbers (the ``Natural`` type).

First case: here's a typeclass to represent the fact
that a ``Foo`` can always be thought of as a ``Natural``:

```haskell
class AsNatural1 a where
    nat1 :: Lens' a Natural

data Foo = Foo {
  _fooName :: String
, _fooNat  :: Natural
} deriving Show

makeLenses ''Foo

instance AsNatural1 Foo where
  nat1 = fooNat :: Lens' Foo Natural
```

Second case: a natural is a natural by definition.

```haskell
instance AsNatural1 Natural where
  nat1 = id
```

Third case: an ``Integer`` might be a ``Natural``. The previous typeclasses used a ``Lens'`` but here
we need a ``Prism'``:

```
class AsNatural2 a where
    nat2 :: Prism' a Natural

instance AsNatural2 Integer where
  nat2 = prism' toInteger (\n -> if n >= 0 then (Just . fromInteger) n else Nothing)
```

We are doing much the same thing, and if we compare the two
typeclasses the difference is in the type of "optical" thing
being used (a lens or a prism):

```
class AsNatural1 a where
    nat1 :: Lens' a Natural

class AsNatural2 a where
    nat2 :: Prism' a Natural
```

It turns out that the type to use is ``Optic'``:

class AsNatural p f s where
  natural :: Optic' p f s Natural

(We get the extra parameters ``p`` and ``f`` which seem to be unavoidable.)

Now we can do all of the previous definitions using the single typeclass:

```
-- Lens into Foo:

instance (p ~ (->), Functor f) => AsNatural p f Foo where
  natural = fooNat :: Lens' Foo Natural

-- Natural is a Natural:

instance AsNatural p f Natural where
  natural = id

-- An Integer might be a natural:

instance (Choice p, Applicative f) => AsNatural p f Integer where
  natural = prism' toInteger (\n -> if n >= 0 then (Just . fromInteger) n else Nothing)
```

Now we can work with a ``Foo``, a ``Natural``, or
an ``Integer`` as a ``Natural`` by using the single
optical ``natural``:

```
main3 :: IO ()
main3 = do
  -- Underlying thing is a Lens:
  print $ (Foo "name" 34) ^. natural
  print $ (Foo "name" 34) ^. natural + 1
  print $ (42 :: Natural) ^. natural + 1

  -- Underlying thing is a Prism (hence the applicative form):
  print $ (+1) <$> ((50 :: Integer)  ^? natural)
  print $ (+1) <$> ((-99 :: Integer) ^? natural)
```

Output:

```
*LensHas> main3
34
35
43
Just 51
Nothing
```

## Credit

The ``AsNatural`` type is a simplified version of the ``As...`` typeclasses in the [coordinate package](http://hackage.haskell.org/package/coordinate), e.g. [AsMinutes](http://hackage.haskell.org/package/coordinate-0.0.18/docs/Data-Geo-Coordinate-Minutes.html#t:AsMinutes). Thanks to Tony Morris on #haskell.au for helping with my changing-API question and pointing out the ``As...`` typeclasses. Also see the IRC logs in [coordinate/etc](https://github.com/NICTA/coordinate/tree/master/etc) where [Ed Kmett](https://twitter.com/kmett) explains some things about Optic.
