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

Short note on <a href="https://hackage.haskell.org/package/base-4.9.1.0/docs/Data-Proxy.html">Data.Proxy</a>
based on <a href="http://stackoverflow.com/questions/27044209/haskell-why-use-proxy/27047260#27047260">this Stackoverflow answer</a>. 

First, a few imports: 

{% highlight haskell %}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Proxy where

import Data.Proxy
import Text.Read
{% endhighlight %}

Suppose we want to check if some fuzzy real world data can be read as certain concrete types. We could write
a few helper functions using <a href="https://hackage.haskell.org/package/base-4.9.1.0/docs/Text-Read.html#v:readMaybe">readMaybe</a>: 

{% highlight haskell %}
readableAsInt :: String -> Bool
readableAsInt s
  = case readMaybe s of
      Just (_ :: Int) -> True
      _               -> False

readableAsDouble :: String -> Bool
readableAsDouble s
  = case readMaybe s of
      Just (_ :: Double) -> True
      _                  -> False

readableAsBool :: String -> Bool
readableAsBool s
  = case readMaybe s of
      Just (_ :: Bool) -> True
      _                -> False
{% endhighlight %}

These are all basically the same. How to generalise? Let's try a typeclass. 

{% highlight haskell %}
class ReadableAs t where
   readableAs :: String -> Bool
{% endhighlight %}

This doesn't work since ``readableAs`` doesn't depend on the type ``t``: 

```
    The class method ‘readableAs’
    mentions none of the type or kind variables of the class ‘ReadableAs t’
    When checking the class method: readableAs :: String -> Bool
    In the class declaration for ‘ReadableAs’
Failed, modules loaded: none.
```

So put the type in: 

{% highlight haskell %}
class ReadableAs' t where
   readableAs' :: t -> String -> Bool
{% endhighlight %}

This compiles, so let's write some instances: 

{% highlight haskell %}
instance ReadableAs' Int where
  readableAs' _ s
     = case readMaybe s of
         Just (_ :: Int) -> True
         _               -> False

instance ReadableAs' Double where
  readableAs' _ s
     = case readMaybe s of
         Just (_ :: Double) -> True
         _                  -> False

instance ReadableAs' Bool where
  readableAs' _ s
     = case readMaybe s of
         Just (_ :: Bool) -> True
         _                -> False
{% endhighlight %}

Using it is clunky since we have to come up with
a concrete value for the first argument: 

```
> readableAs' (0::Int) "0"
True
> readableAs' (0::Double) "0"
True
```

For some types we could
use <a href="https://hackage.haskell.org/package/data-default">Data.Default</a>
for this placeholder value. But for other types nothing will make sense. How do we choose
a default value for ``Foo``? 

{% highlight haskell %}
data Foo = Cat | Dog
{% endhighlight %}

Haskell has non-strict evaluation so we can use ``undefined``, but, ugh. Bad idea. 

```
 > readableAs' (undefined::Int) "0"
 True
```

So let's try out ``Proxy``. It has a single constructor and a free type variable that we can set: 

```
> :t Proxy
Proxy :: Proxy t

> Proxy :: Proxy Bool
Proxy
> Proxy :: Proxy Int
Proxy
> Proxy :: Proxy Double
Proxy
```

Let's use ``Proxy t`` instead of ``t``: 

{% highlight haskell %}
class ReadableAsP t where
   readableAsP :: Proxy t -> String -> Bool

instance ReadableAsP Int where
  readableAsP _ s
     = case readMaybe s of
         Just (_ :: Int) -> True
         _               -> False

instance ReadableAsP Double where
  readableAsP _ s
     = case readMaybe s of
         Just (_ :: Double) -> True
         _                  -> False

instance ReadableAsP Bool where
  readableAsP _ s
     = case readMaybe s of
         Just (_ :: Bool) -> True
         _                -> False
{% endhighlight %}

This works, and we don't have to come up with the unused concrete value: 

```
> readableAsP (Proxy :: Proxy Bool) "0"
False
> readableAsP (Proxy :: Proxy Bool) "True"
True
> readableAsP (Proxy :: Proxy Int) "0"
True
> readableAsP (Proxy :: Proxy Double) "0"
True
> readableAsP (Proxy :: Proxy Double) "0.0"
True
```

Still, there's a lot of duplication in the class and instances. We can do away
with the class entirely. With the ``ScopedTypeVariables `` language extension
and the ``forall``, the ``t`` in the type signature
can be referred to in the body: 

{% highlight haskell %}
readableAs :: forall t. Read t => Proxy t -> String -> Bool
readableAs _ s
     = case readMaybe s of
         Just (_ :: t) -> True
         _             -> False
{% endhighlight %}

```
> readableAs (Proxy :: Proxy Int) "0"
True
> readableAs (Proxy :: Proxy Int) "foo"
False
```
