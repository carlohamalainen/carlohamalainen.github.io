---
id: 783
title: Data.Proxy
date: 2017-03-25T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2017/03/25/data-proxy/
permalink: /2017/03/25/data-proxy/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
Short note on [Data.Proxy](https://hackage.haskell.org/package/base-4.9.1.0/docs/Data-Proxy.html) based on [this Stackoverflow answer](http://stackoverflow.com/questions/27044209/haskell-why-use-proxy/27047260#27047260).

First, a few imports: 

<pre>&gt; {-# LANGUAGE RankNTypes          #-}
&gt; {-# LANGUAGE ScopedTypeVariables #-}
&gt;
&gt; module Proxy where
&gt;
&gt; import Data.Proxy
&gt; import Text.Read
</pre>

Suppose we want to check if some fuzzy real world data can be read as certain concrete types. We could write a few helper functions using [readMaybe](https://hackage.haskell.org/package/base-4.9.1.0/docs/Text-Read.html#v:readMaybe): 

<pre>&gt; readableAsInt :: String -&gt; Bool
&gt; readableAsInt s
&gt;   = case readMaybe s of
&gt;       Just (_ :: Int) -&gt; True
&gt;       _               -&gt; False
&gt;
&gt; readableAsDouble :: String -&gt; Bool
&gt; readableAsDouble s
&gt;   = case readMaybe s of
&gt;       Just (_ :: Double) -&gt; True
&gt;       _                  -&gt; False
&gt;
&gt; readableAsBool :: String -&gt; Bool
&gt; readableAsBool s
&gt;   = case readMaybe s of
&gt;       Just (_ :: Bool) -&gt; True
&gt;       _                -&gt; False
</pre>

These are all basically the same. How to generalise? Let’s try a typeclass. 

<pre>&gt; class ReadableAs t where
&gt;    readableAs :: String -&gt; Bool
</pre>

This doesn’t work since readableAs doesn’t depend on the type t: 

<pre>The class method ‘readableAs’
    mentions none of the type or kind variables of the class ‘ReadableAs t’
    When checking the class method: readableAs :: String -&gt; Bool
    In the class declaration for ‘ReadableAs’
Failed, modules loaded: none.
</pre>

So put the type in: 

<pre>&gt; class ReadableAs' t where
&gt;    readableAs' :: t -&gt; String -&gt; Bool
</pre>

This compiles, so let’s write some instances: 

<pre>&gt; instance ReadableAs' Int where
&gt;   readableAs' _ s
&gt;      = case readMaybe s of
&gt;          Just (_ :: Int) -&gt; True
&gt;          _               -&gt; False
&gt;
&gt; instance ReadableAs' Double where
&gt;   readableAs' _ s
&gt;      = case readMaybe s of
&gt;          Just (_ :: Double) -&gt; True
&gt;          _                  -&gt; False
&gt;
&gt; instance ReadableAs' Bool where
&gt;   readableAs' _ s
&gt;      = case readMaybe s of
&gt;          Just (_ :: Bool) -&gt; True
&gt;          _                -&gt; False
</pre>

Using it is clunky since we have to come up with a concrete value for the first argument: 

<pre>&gt; readableAs' (0::Int) "0"
 True
 &gt; readableAs' (0::Double) "0"
 True
</pre>

For some types we could use [Data.Default](https://hackage.haskell.org/package/data-default) for this placeholder value. But for other types nothing will make sense. How do we choose a default value for Foo? 

<pre>&gt; data Foo = Cat | Dog
</pre>

Haskell has non-strict evaluation so we can use undefined, but, ugh. Bad idea. 

<pre>&gt; readableAs' (undefined::Int) "0"
 True
</pre>

So let’s try out Proxy. It has a single constructor and a free type variable that we can set: 

<pre>&gt; :t Proxy
 Proxy :: Proxy t
</pre>

<pre>&gt; Proxy :: Proxy Bool
 Proxy
 &gt; Proxy :: Proxy Int
 Proxy
 &gt; Proxy :: Proxy Double
 Proxy
</pre>

Let’s use Proxy t instead of t: 

<pre>&gt; class ReadableAsP t where
&gt;    readableAsP :: Proxy t -&gt; String -&gt; Bool
&gt;
&gt; instance ReadableAsP Int where
&gt;   readableAsP _ s
&gt;      = case readMaybe s of
&gt;          Just (_ :: Int) -&gt; True
&gt;          _               -&gt; False
&gt;
&gt; instance ReadableAsP Double where
&gt;   readableAsP _ s
&gt;      = case readMaybe s of
&gt;          Just (_ :: Double) -&gt; True
&gt;          _                  -&gt; False
&gt;
&gt; instance ReadableAsP Bool where
&gt;   readableAsP _ s
&gt;      = case readMaybe s of
&gt;          Just (_ :: Bool) -&gt; True
&gt;          _                -&gt; False
</pre>

This works, and we don’t have to come up with the unused concrete value: 

<pre>&gt; readableAsP (Proxy :: Proxy Bool) "0"
 False
 &gt; readableAsP (Proxy :: Proxy Bool) "True"
 True
 &gt; readableAsP (Proxy :: Proxy Int) "0"
 True
 &gt; readableAsP (Proxy :: Proxy Double) "0"
 True
 &gt; readableAsP (Proxy :: Proxy Double) "0.0"
 True
</pre>

Still, there’s a lot of duplication in the class and instances. We can do away with the class entirely. With the ScopedTypeVariables language extension and the forall, the t in the type signature can be referred to in the body: 

<pre>&gt; readableAs :: forall t. Read t =&gt; Proxy t -&gt; String -&gt; Bool
&gt; readableAs _ s
&gt;      = case readMaybe s of
&gt;          Just (_ :: t) -&gt; True
&gt;          _             -&gt; False
</pre>

<pre>&gt; readableAs (Proxy :: Proxy Int) "0"
 True
 &gt; readableAs (Proxy :: Proxy Int) "foo"
 False
</pre>

**Archived comments**



* * *

### [Franklin Chen](http://conscientiousprogrammer.com/)

#### 2017-03-25 02:23:07.94742 UTC

This can also now be done without Proxy, thanks to explicit type application: 

<pre>{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

import Data.Maybe (isJust)

readableAs :: forall t. Read t =&gt; String -&gt; Bool
readableAs = isJust @t . readMaybe

example1 = readableAs @Int "0"
example2 = readableAs @Int "foo"
example3 = readableAs @Double "0.1"
</pre>