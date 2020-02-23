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

Literate Haskell source for this post: <https://github.com/carlohamalainen/playground/tree/master/haskell/lens-has>.

First, some extensions and imports.

<pre>&gt; {-# LANGUAGE GADTs                        #-}
&gt; {-# LANGUAGE FlexibleInstances            #-}
&gt; {-# LANGUAGE MultiParamTypeClasses        #-}
&gt; {-# LANGUAGE TemplateHaskell              #-}
</pre>

<pre>&gt; module LensHas where
</pre>

<pre>&gt; import Control.Applicative
&gt; import Control.Lens
&gt; import Numeric.Natural
</pre>

## Introduction 

Suppose we are working with a database service that stores files. Perhaps we communicate with it via a REST API. A file stored in the system has a location, which is a FilePath:

<pre>&gt; type Location = FilePath
</pre>

We need to keep track of a few other things like the parent (referring to a collection of files) and a hash of the file. For simplicity I’ll make those two fields Strings since the details aren’t important to us here. 

<pre>&gt; data DataFile = DataFile {
&gt;     _dataFileLocation :: Location
&gt;   , _dataFileParent   :: String
&gt;   , _dataFileHash     :: String
&gt; } deriving Show
</pre>

(Ignore the underscores if you haven’t used lenses before.) 

After some time the API changes and we need to keep track of some different fields, so our data type changes to:

<pre>&gt; data DataFile2 = DataFile2 {
&gt;     _dataFile2Location   :: Location
&gt;   , _dataFile2Parent     :: String
&gt;   , _dataFile2OtherField :: Float -- new field
&gt;                                   -- hash is not here anymore
&gt; } deriving Show
</pre>

For compatibility we’d like to keep both definitions around, perhaps allowing the user to choose the v1 or v2 API with a configuration option. So how do we deal with our code that has to use DataFile or DataFile2? One option is to use a sum type: 

<pre>&gt; data DataFileSum = DFS1 DataFile | DFS2 DataFile2
</pre>

Any function that uses a DataFile must instead use DataFileSum and do case analysis on whether it is a v1 or v2. 

In my particular situation I had a number of functions that used just the Location part of the type. Is there a way to avoid the sum type? 

## Setter/Getter typeclasses 

Use typeclasses to represent setting or getting the location value: 

<pre>&gt; class SetLocation a where
&gt;   setLocation :: a -&gt; Location -&gt; a
</pre>

<pre>&gt; class GetLocation a where
&gt;   getLocation :: a -&gt; Location
</pre>

Write the instance definitions for each case: 

<pre>&gt; instance SetLocation DataFile where
&gt;   setLocation d newLocation = d { _dataFileLocation = newLocation }
&gt;
&gt; instance GetLocation DataFile where
&gt;   getLocation = _dataFileLocation
</pre>

<pre>&gt; instance SetLocation DataFile2 where
&gt;   setLocation d newLocation = d { _dataFile2Location = newLocation }
&gt;
&gt; instance GetLocation DataFile2 where
&gt;   getLocation = _dataFile2Location
</pre>

Now we use the general getLocation and setLocation functions instead of the specific data constructors of DataFile and DataFile2:

<pre>&gt; main1 = do
&gt;   let df = DataFile "/foo/bar.txt" "something" "700321159acb26a5fd6d5ce0116a6215"
&gt;
&gt;   putStrLn $ "Original data file: " ++ show df
&gt;   putStrLn $ "Location in original: " ++ getLocation df
&gt;
&gt;   let df' = setLocation df "/blah/bar.txt"
&gt;
&gt;   putStrLn $ "Updated data file:    " ++ getLocation df'
</pre>

A function that uses a datafile can now be agnostic about which one it is, as long as the typeclass constraint is satisfied so that it has the appropriate getter/setter: 

<pre>&gt; doSomething :: GetLocation a =&gt; a -&gt; IO ()
&gt; doSomething d = print $ getLocation d
</pre>

Using doSomething: 

<pre>*LensHas&gt; doSomething $ DataFile "/foo/bar.txt" "parent" "12345"
"/foo/bar.txt"

*LensHas&gt; doSomething $ DataFile2 "/foo/bar.txt" "parent" 42.2
"/foo/bar.txt"
</pre>

## Lenses 

Lenses already deal with the concept of getters and setters, so let’s try to replicate the previous code in that framework. 

First, make lenses for the two data types (this uses Template Haskell): 

<pre>&gt; makeLenses ''DataFile
&gt; makeLenses ''DataFile2
</pre>

Instead of type classes for setting and getting, make a single type class that represents the fact that a thing _has_ a location.

<pre>&gt; class HasLocation a where
&gt;     location :: Lens' a Location
</pre>

For the instance definitions we can use the lenses that were automatically made for us by the earlier makeLenses lines: 

<pre>&gt; instance HasLocation DataFile where
&gt;     location = dataFileLocation :: Lens' DataFile Location
&gt;
&gt; instance HasLocation DataFile2 where
&gt;     location = dataFile2Location :: Lens' DataFile2 Location
</pre>

Here is main1 rewritten to use the location lens: 

<pre>&gt; main2 = do
&gt;   let df = DataFile "/foo/bar.txt" "something" "700321159acb26a5fd6d5ce0116a6215"
&gt;
&gt;   putStrLn $ "Original data file: " ++ show df
&gt;   putStrLn $ "Location in original: " ++ df^.location
&gt;
&gt;   let df' = df & location .~ "/blah/bar.txt"
&gt;
&gt;   putStrLn $ "Updated data file:    " ++ getLocation df'
</pre>

If you haven’t used lenses before the operators like ^. might look insane, but there is a pattern to them. Check out [http://intolerable.me/lens-operators-intro](http://intolerable.me/lens-operators-intro/) for an excellent guide with examples.

One benefit of the lens approach is that we don’t have to manually write the setters and getters, as they come for free from the lenses for the original two data types. Another benefit is that lenses compose, so if the Location type was more than just a string, we wouldn’t have to manually deal with the composition of getLocation with getSubPartOfLocation and so on. 

The doSomething function can be rewritten using the HasLocation typeclass: 

<pre>&gt; doSomething' :: HasLocation a =&gt; a -&gt; IO ()
&gt; doSomething' d = print $ d^.location
</pre>

## Generalising HasLocation 

Let’s generalise the HasLocation typeclass. Consider natural numbers (the Natural type).

First case: here’s a typeclass to represent the fact that a Foo can always be thought of as a Natural: 

<pre>&gt; class AsNatural1 a where
&gt;     nat1 :: Lens' a Natural
</pre>

<pre>&gt; data Foo = Foo {
&gt;   _fooName :: String
&gt; , _fooNat  :: Natural
&gt; } deriving Show
&gt;
&gt; makeLenses ''Foo
</pre>

<pre>&gt; instance AsNatural1 Foo where
&gt;   nat1 = fooNat :: Lens' Foo Natural
</pre>

Second case: a natural is a natural by definition. 

<pre>&gt; instance AsNatural1 Natural where
&gt;   nat1 = id
</pre>

Third case: an Integer might be a Natural. The previous typeclasses used a Lens’ but here we need a Prism’: 

<pre>&gt; class AsNatural2 a where
&gt;     nat2 :: Prism' a Natural
</pre>

<pre>&gt; instance AsNatural2 Integer where
&gt;   nat2 = prism' toInteger (n -&gt; if n &gt;= 0 then (Just . fromInteger) n else Nothing)
</pre>

We are doing much the same thing, and if we compare the two typeclasses the difference is in the type of “optical” thing being used (a lens or a prism): 

<pre>&gt; class AsNatural1 a where
&gt;     nat1 :: Lens' a Natural
&gt;
&gt; class AsNatural2 a where
&gt;     nat2 :: Prism' a Natural
</pre>

It turns out that the type to use is Optic’: 

<pre>&gt; class AsNatural p f s where
&gt;   natural :: Optic' p f s Natural
</pre>

(We get the extra parameters p and f which seem to be unavoidable.) 

Now we can do all of the previous definitions using the single typeclass: 

<pre>&gt; -- Lens into Foo:
&gt;
&gt; instance (p ~ (-&gt;), Functor f) =&gt; AsNatural p f Foo where
&gt;   natural = fooNat :: Lens' Foo Natural
&gt;
&gt; -- Natural is a Natural:
&gt;
&gt; instance AsNatural p f Natural where
&gt;   natural = id
&gt;
&gt; -- An Integer might be a natural:
&gt;
&gt; instance (Choice p, Applicative f) =&gt; AsNatural p f Integer where
&gt;   natural = prism' toInteger (n -&gt; if n &gt;= 0 then (Just . fromInteger) n else Nothing)
</pre>

Now we can work with a Foo, a Natural, or an Integer as a Natural by using the single optical natural: 

<pre>&gt; main3 :: IO ()
&gt; main3 = do
&gt;   -- Underlying thing is a Lens:
&gt;   print $ (Foo "name" 34) ^. natural
&gt;   print $ (Foo "name" 34) ^. natural + 1
&gt;   print $ (42 :: Natural) ^. natural + 1
&gt;
&gt;   -- Underlying thing is a Prism (hence the applicative form):
&gt;   print $ (+1)  ((50 :: Integer)  ^? natural)
&gt;   print $ (+1)  ((-99 :: Integer) ^? natural)
</pre>

Output: 

<pre>*LensHas&gt; main3
34
35
43
Just 51
Nothing
</pre>

## Credit 

The AsNatural type is a simplified version of the “As…” typeclasses in the [coordinate package](http://hackage.haskell.org/package/coordinate), e.g. [AsMinutes](http://hackage.haskell.org/package/coordinate-0.0.18/docs/Data-Geo-Coordinate-Minutes.html#t:AsMinutes). Thanks to Tony Morris on #haskell.au for helping with my changing-API question and pointing out the “As…” typeclasses. Also see the IRC logs in [coordinate/etc](https://github.com/NICTA/coordinate/tree/master/etc) where [Ed Kmett](https://twitter.com/kmett) explains some things about Optic.