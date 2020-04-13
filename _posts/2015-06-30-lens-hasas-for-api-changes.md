---
id: 810
title: Lens Has/As for API changes
date: 2015-06-30T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2015/06/30/lens-hasas-for-api-changes/
permalink: /2015/06/30/lens-hasas-for-api-changes/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---

Tinkering with lenses to deal with API changes. 

Literate Haskell source for this post: <a href="https://github.com/carlohamalainen/playground/tree/master/haskell/lens-has">https://github.com/carlohamalainen/playground/tree/master/haskell/lens-has</a>. 

First, some extensions and imports. 

{% highlight haskell %}
{-# LANGUAGE GADTs                        #-}
{-# LANGUAGE FlexibleInstances            #-}
{-# LANGUAGE MultiParamTypeClasses        #-}
{-# LANGUAGE TemplateHaskell              #-}

module LensHas where

import Control.Applicative
import Control.Lens
import Numeric.Natural
{% endhighlight %}

## Introduction 

Suppose we are working with a database service that stores files. Perhaps we communicate with it via a REST API. A file stored in the system has a location, which is a ``FilePath``: 

{% highlight haskell %}
type Location = FilePath
{% endhighlight %}

We need to keep track of a few other things like the parent (referring to a collection of files) and a hash of the file. For simplicity I'll make those two fields ``String``s since the details aren't important to us here. 

{% highlight haskell %}
data DataFile = DataFile {
    _dataFileLocation :: Location
  , _dataFileParent   :: String
  , _dataFileHash     :: String
} deriving Show
{% endhighlight %}

(Ignore the underscores if you haven't used lenses before.) 

After some time the API changes and we need to keep track of some
different fields, so our data type changes to: 

{% highlight haskell %}
data DataFile2 = DataFile2 {
    _dataFile2Location   :: Location
  , _dataFile2Parent     :: String
  , _dataFile2OtherField :: Float -- new field
                                  -- hash is not here anymore
} deriving Show
{% endhighlight %}

For compatibility we'd like to keep both definitions around, perhaps allowing the user
to choose the v1 or v2 API with a configuration option. So how do we deal with our code that
has to use ``DataFile`` or ``DataFile2``? One option is to use a sum type: 

{% highlight haskell %}
data DataFileSum = DFS1 DataFile | DFS2 DataFile2
{% endhighlight %}

Any function that uses a ``DataFile`` must instead
use ``DataFileSum`` and do case analysis on whether it is
a v1 or v2. 

In my particular situation I had a number of functions that used
just the ``Location`` part of the type. Is there a way to
avoid the sum type? 

## Setter/Getter typeclasses 

Use typeclasses to represent setting or getting the location value: 

{% highlight haskell %}
class SetLocation a where
  setLocation :: a -> Location -> a

class GetLocation a where
  getLocation :: a -> Location
{% endhighlight %}

Write the instance definitions for each case: 

{% highlight haskell %}
instance SetLocation DataFile where
  setLocation d newLocation = d { _dataFileLocation = newLocation }

instance GetLocation DataFile where
  getLocation = _dataFileLocation

instance SetLocation DataFile2 where
  setLocation d newLocation = d { _dataFile2Location = newLocation }

instance GetLocation DataFile2 where
  getLocation = _dataFile2Location
{% endhighlight %}

Now we use the general ``getLocation`` and ``setLocation`` functions instead of
the specific data constructors of ``DataFile`` and ``DataFile2``: 

{% highlight haskell %}
main1 = do
  let df = DataFile "/foo/bar.txt" "something" "700321159acb26a5fd6d5ce0116a6215"

  putStrLn $ "Original data file: " ++ show df
  putStrLn $ "Location in original: " ++ getLocation df

  let df' = setLocation df "/blah/bar.txt"

  putStrLn $ "Updated data file:    " ++ getLocation df'
{% endhighlight %}

A function that uses a datafile can now be agnostic about which one it is, as long as the typeclass
constraint is satisfied so that it has the appropriate getter/setter: 

{% highlight haskell %}
doSomething :: GetLocation a => a -> IO ()
doSomething d = print $ getLocation d
{% endhighlight %}

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

```
makeLenses ''DataFile
makeLenses ''DataFile2
```

Instead of type classes for setting and getting, make a single type class
that represents the fact that a thing _has_ a location.

{% highlight haskell %}
class HasLocation a where
    location :: Lens' a Location
{% endhighlight %}

For the instance definitions we can use the lenses that were automatically made for us by the earlier ``makeLenses`` lines: 

{% highlight haskell %}
instance HasLocation DataFile where
    location = dataFileLocation :: Lens' DataFile Location

instance HasLocation DataFile2 where
    location = dataFile2Location :: Lens' DataFile2 Location
{% endhighlight %}

Here is ``main1`` rewritten to use the ``location`` lens: 

{% highlight haskell %}
main2 = do
  let df = DataFile "/foo/bar.txt" "something" "700321159acb26a5fd6d5ce0116a6215"

  putStrLn $ "Original data file: " ++ show df
  putStrLn $ "Location in original: " ++ df^.location

  let df' = df & location .~ "/blah/bar.txt"

  putStrLn $ "Updated data file:    " ++ getLocation df'
{% endhighlight %}

If you haven't used lenses before the operators like ``^.`` might look insane, but there is a pattern to them. Check out <a href="http://intolerable.me/lens-operators-intro/">http://intolerable.me/lens-operators-intro</a> for an excellent guide with examples. 

One benefit of the lens approach is that we don't have to
manually write the setters and getters, as they come for free
from the lenses for the original two data types. Another benefit
is that lenses compose, so if the ``Location`` type was more than just a string,
we wouldn't have to manually deal with the composition of ``getLocation`` with ``getSubPartOfLocation`` and so on. 

The ``doSomething`` function can be rewritten using the ``HasLocation`` typeclass: 

{% highlight haskell %}
doSomething' :: HasLocation a => a -> IO ()
doSomething' d = print $ d^.location
{% endhighlight %}

## Generalising HasLocation 

Let's generalise the ``HasLocation`` typeclass. Consider natural numbers (the ``Natural`` type). 

First case: here's a typeclass to represent the fact
that a ``Foo`` can always be thought of as a ``Natural``:

{% highlight haskell %}
class AsNatural1 a where
    nat1 :: Lens' a Natural

data Foo = Foo {
  _fooName :: String
, _fooNat  :: Natural
} deriving Show

makeLenses ''Foo

instance AsNatural1 Foo where
  nat1 = fooNat :: Lens' Foo Natural
{% endhighlight %}

Second case: a natural is a natural by definition. 

{% highlight haskell %}
instance AsNatural1 Natural where
  nat1 = id
{% endhighlight %}

Third case: an ``Integer`` might be a ``Natural``. The previous typeclasses used a ``Lens'`` but here
we need a ``Prism'``: 

{% highlight haskell %}
class AsNatural2 a where
    nat2 :: Prism' a Natural

instance AsNatural2 Integer where
  nat2 = prism' toInteger (\n -> if n >= 0 then (Just . fromInteger) n else Nothing)
{% endhighlight %}

We are doing much the same thing, and if we compare the two
typeclasses the difference is in the type of "optical" thing
being used (a lens or a prism): 

{% highlight haskell %}
class AsNatural1 a where
    nat1 :: Lens' a Natural

class AsNatural2 a where
    nat2 :: Prism' a Natural
{% endhighlight %}

It turns out that the type to use is ``Optic'``: 

class AsNatural p f s where
  natural :: Optic' p f s Natural

(We get the extra parameters ``p`` and ``f`` which seem to be unavoidable.) 

Now we can do all of the previous definitions using the single typeclass: 

{% highlight haskell %}
-- Lens into Foo:

instance (p ~ (->), Functor f) => AsNatural p f Foo where
  natural = fooNat :: Lens' Foo Natural

-- Natural is a Natural:

instance AsNatural p f Natural where
  natural = id

-- An Integer might be a natural:

instance (Choice p, Applicative f) => AsNatural p f Integer where
  natural = prism' toInteger (\n -> if n >= 0 then (Just . fromInteger) n else Nothing)
{% endhighlight %}

Now we can work with a ``Foo``, a ``Natural``, or
an ``Integer`` as a ``Natural`` by using the single
optical ``natural``: 

{% highlight haskell %}
main3 :: IO ()
main3 = do
  -- Underlying thing is a Lens:
  print $ (Foo "name" 34) ^. natural
  print $ (Foo "name" 34) ^. natural + 1
  print $ (42 :: Natural) ^. natural + 1

  -- Underlying thing is a Prism (hence the applicative form):
  print $ (+1) <$> ((50 :: Integer)  ^? natural)
  print $ (+1) <$> ((-99 :: Integer) ^? natural)
{% endhighlight %}

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

The ``AsNatural`` type is a simplified version of the ``As...`` typeclasses in the <a href="http://hackage.haskell.org/package/coordinate">coordinate package</a>, e.g. <a href="http://hackage.haskell.org/package/coordinate-0.0.18/docs/Data-Geo-Coordinate-Minutes.html#t:AsMinutes">AsMinutes</a>. Thanks to Tony Morris on #haskell.au for helping with my changing-API question and pointing out the ``As...`` typeclasses. Also see the IRC logs in <a href="https://github.com/NICTA/coordinate/tree/master/etc">coordinate/etc</a> where <a href="https://twitter.com/kmett">Ed Kmett</a> explains some things about Optic. 
