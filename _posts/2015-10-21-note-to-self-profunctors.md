---
id: 741
title: 'Note to self: profunctors'
date: 2015-10-21T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2015/10/21/note-to-self-profunctors/
permalink: /2015/10/21/note-to-self-profunctors/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---

Note to self about deriving the ``Profunctor`` typeclass. Source is here: <a href="https://github.com/carlohamalainen/playground/tree/master/haskell/profunctors">here</a>. 

This is a literate Haskell file, and it can be built using <a href="https://github.com/commercialhaskell/stack">Stack</a>: 

```
git clone https://github.com/carlohamalainen/playground.git
cd playground/haskell/profunctors
stack build
```

Then use ``stack ghci`` instead of ``cabal repl``. The main executable is
in a path like ``./.stack-work/install/x86_64-linux/lts-3.6/7.10.2/bin/profunctors-exe``.  

This blog post follows some of the examples from <a href="https://www.fpcomplete.com/user/liyang/profunctors">I love profunctors.</a> 

First, some extensions and imports: 

{% highlight haskell %}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Profunctors where

import Control.Applicative
import Data.Char
import Data.Functor.Constant
import Data.Functor.Identity
import Data.Tuple (swap)
import qualified Data.Map as M

main = print "boo"
{% endhighlight %}

## Motivating example 

The basic problem here is to write a function that
capitalizes each word in a string. First, write a
function that capitalizes a single word: 

{% highlight haskell %}
capWord :: String -> String
capWord [] = []
capWord (h:t) = (toUpper h):(map toLower t)
{% endhighlight %}

The straightforward solution (ignoring the loss of extra spaces
between words since ``unwords . words`` is not an isomorphism)
is to use this composition: 

{% highlight haskell %}
capitalize :: String -> String
capitalize = unwords . (map capWord) . words
{% endhighlight %}

Example output: 

```
*Profunctors> capitalize "hey yo WHAT DID THIS          DO?"
"Hey Yo What Did This Do?"
```

Why stop here? Let's generalise the ``capitalize``
function by factoring out the ``words``
and ``unwords`` functions. Call them ``w``
and ``u`` and make them arguments: 

{% highlight haskell %}
capitalize1 :: (String -> [String]) -> ([String] -> String) -> String -> String
capitalize1 w u = u . (map capWord) . w
{% endhighlight %}

Now, ``capitalize ≡ capitalize1 words unwords``. 

We may as well factor out ``map capWord`` as well: 

{% highlight haskell %}
capitalize2 :: (String -> [String])
             -> ([String] -> String)
             -> ([String] -> [String])
             -> String -> String
capitalize2 w u f = u . f . w
{% endhighlight %}

We have: ``capitalize ≡ capitalize2 words unwords (map capWord)``. 

Now look at the types - there is no reason to be restricted
to ``String`` and ``[String]`` so use the most
general types that make the composition ``u . f . w`` work: 

```
     w          f          u
c -------> d -------> b -------> d
```

so ``w :: c -> d`` and similar for ``f`` and ``u``.  This lets us write 

{% highlight haskell %}
capitalize3 :: (c -> a)
            -> (b -> d)
            -> (a -> b)
            -> (c -> d)
capitalize3 w u f = u . f . w
{% endhighlight %}

Next, we can generalize the type of ``f``. To help with this step, recall
that ``->`` is a functor (there is an instance ``Functor (->)``) so write
the last two types in the signature with prefix notation: 

{% highlight haskell %}
capitalize3' :: (c -> a)
             -> (b -> d)
             -> (->) a b
             -> (->) c d
capitalize3' w u f = u . f . w
{% endhighlight %}

Now we can use a general functor ``h`` instead of ``->``: 

{% highlight haskell %}
capitalize4 :: (c -> a)
            -> (b -> d)
            -> h a b -- was (->) a b
            -> h c d -- was (->) c d
capitalize4 w u f = u . f . w
{% endhighlight %}

Naturally this won't work because the type signature has the
functor ``h`` but the body of ``capitalize4``
is using function composition (the ``.``) as the type error
shows: 

```
| Couldn't match type ‘h’ with ‘(->)’
||   ‘h’ is a rigid type variable bound by
||       the type signature for
||         capitalize3' :: (c -> a) -> (b -> d) -> h a b -> h c d
|| Expected type: h c d
||   Actual type: c -> d
```

Fortunately for us, we can make a typeclass that captures the behaviour that we want.
We have actually arrived at the definition of a profunctor. 

{% highlight haskell %}
class Profunctor f where
  dimap :: (c -> a) -> (b -> d) -> f a b -> f c d

instance Profunctor (->) where
  dimap :: (c -> a) -> (b -> d) -> (a -> b) -> c -> d
  dimap h g k = g . k . h
{% endhighlight %}

Now we can write the capitalize function using
a typeclass constraint on ``Profunctor`` which lets us
use the ``dimap`` function instead
of explicit function composition: 

{% highlight haskell %}
capitalize5 :: String -> String
capitalize5 s = dimap words unwords (map capWord) s
{% endhighlight %}

This is overkill for the capitalization problem, but it shows how structure
can come out of simple problems if you keep hacking away. 
