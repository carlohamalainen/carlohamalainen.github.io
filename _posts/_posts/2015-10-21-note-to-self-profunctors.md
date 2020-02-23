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
Note to self about deriving the Profunctor typeclass. Source is here: [here](https://github.com/carlohamalainen/playground/tree/master/haskell/profunctors).

This is a literate Haskell file, and it can be built using [Stack](https://github.com/commercialhaskell/stack): 

<pre>git clone https://github.com/carlohamalainen/playground.git
cd playground/haskell/profunctors
stack build
</pre>

Then use stack ghci instead of cabal repl. The main executable is in a path like ./.stack-work/install/x86_64-linux/lts-3.6/7.10.2/bin/profunctors-exe.

This blog post follows some of the examples from [I love profunctors.](https://www.fpcomplete.com/user/liyang/profunctors) 

First, some extensions and imports: 

<pre>&gt; {-# LANGUAGE MultiParamTypeClasses #-}
&gt; {-# LANGUAGE FlexibleInstances     #-}
&gt; {-# LANGUAGE InstanceSigs          #-}
&gt; {-# LANGUAGE RankNTypes            #-}
&gt; {-# LANGUAGE ScopedTypeVariables   #-}
&gt;
&gt; module Profunctors where
&gt;
&gt; import Control.Applicative
&gt; import Data.Char
&gt; import Data.Functor.Constant
&gt; import Data.Functor.Identity
&gt; import Data.Tuple (swap)
&gt; import qualified Data.Map as M
&gt;
&gt; main = print "boo"
</pre>

## Motivating example 

The basic problem here is to write a function that capitalizes each word in a string. First, write a function that capitalizes a single word: 

<pre>&gt; capWord :: String -&gt; String
&gt; capWord [] = []
&gt; capWord (h:t) = (toUpper h):(map toLower t)
</pre>

The straightforward solution (ignoring the loss of extra spaces between words since unwords . words is not an isomorphism) is to use this composition: 

<pre>&gt; capitalize :: String -&gt; String
&gt; capitalize = unwords . (map capWord) . words
</pre>

Example output: 

<pre>*Profunctors&gt; capitalize "hey yo WHAT DID THIS          DO?"
"Hey Yo What Did This Do?"
</pre>

Why stop here? Let’s generalise the capitalize function by factoring out the words and unwords functions. Call them w and u and make them arguments:

<pre>&gt; capitalize1 :: (String -&gt; [String]) -&gt; ([String] -&gt; String) -&gt; String -&gt; String
&gt; capitalize1 w u = u . (map capWord) . w
</pre>

Now, capitalize ≡ capitalize1 words unwords.

We may as well factor out map capWord as well: 

<pre>&gt; capitalize2 :: (String -&gt; [String])
&gt;              -&gt; ([String] -&gt; String)
&gt;              -&gt; ([String] -&gt; [String])
&gt;              -&gt; String -&gt; String
&gt; capitalize2 w u f = u . f . w
</pre>

We have: capitalize ≡ capitalize2 words unwords (map capWord).

Now look at the types &#8211; there is no reason to be restricted to String and [String] so use the most general types that make the composition u . f . w work: 

<pre>w          f          u
c -------&gt; d -------&gt; b -------&gt; d
</pre>

so w :: c -> d and similar for f and u. This lets us write 

<pre>&gt; capitalize3 :: (c -&gt; a)
&gt;             -&gt; (b -&gt; d)
&gt;             -&gt; (a -&gt; b)
&gt;             -&gt; (c -&gt; d)
&gt; capitalize3 w u f = u . f . w
</pre>

Next, we can generalize the type of f. To help with this step, recall that -> is a functor (there is an instance Functor (->)) so write the last two types in the signature with prefix notation: 

<pre>&gt; capitalize3' :: (c -&gt; a)
&gt;              -&gt; (b -&gt; d)
&gt;              -&gt; (-&gt;) a b
&gt;              -&gt; (-&gt;) c d
&gt; capitalize3' w u f = u . f . w
</pre>

Now we can use a general functor h instead of ->: 

<pre>&gt; capitalize4 :: (c -&gt; a)
&gt;             -&gt; (b -&gt; d)
&gt;             -&gt; h a b -- was (-&gt;) a b
&gt;             -&gt; h c d -- was (-&gt;) c d
&gt; capitalize4 w u f = u . f . w
</pre>

Naturally this won’t work because the type signature has the functor h but the body of capitalize4 is using function composition (the .) as the type error shows:

<pre>| Couldn't match type ‘h’ with ‘(-&gt;)’
||   ‘h’ is a rigid type variable bound by
||       the type signature for
||         capitalize3' :: (c -&gt; a) -&gt; (b -&gt; d) -&gt; h a b -&gt; h c d
|| Expected type: h c d
||   Actual type: c -&gt; d
</pre>

Fortunately for us, we can make a typeclass that captures the behaviour that we want. We have actually arrived at the definition of a profunctor.

<pre>&gt; class Profunctor f where
&gt;   dimap :: (c -&gt; a) -&gt; (b -&gt; d) -&gt; f a b -&gt; f c d
&gt;
&gt; instance Profunctor (-&gt;) where
&gt;   dimap :: (c -&gt; a) -&gt; (b -&gt; d) -&gt; (a -&gt; b) -&gt; c -&gt; d
&gt;   dimap h g k = g . k . h
</pre>

Now we can write the capitalize function using a typeclass constraint on Profunctor which lets us use the dimap function instead of explicit function composition: 

<pre>&gt; capitalize5 :: String -&gt; String
&gt; capitalize5 s = dimap words unwords (map capWord) s
</pre>

This is overkill for the capitalization problem, but it shows how structure can come out of simple problems if you keep hacking away.