---
id: 1136
title: 'QuickCheck&#8217;s CoArbitrary &#8211; generate random functions!'
date: 2018-01-30T22:19:24+00:00
author: Carlo Hamalainen
layout: post
guid: https://carlo-hamalainen.net/?p=1136
permalink: /2018/01/30/quickchecks-coarbitrary-generate-random-functions/
categories:
  - Uncategorized
---
Property based testing is a great way to improve code quality since it captures logical properties of your system. Instead of writing test cases by hand, you capture logical relationships and then let the test framework generate hundreds or thousands of examples for you.

For a general introduction to property based testing (language-independent), try this YOW! Night talk [Property Based Testing Finding More Bugs with Less Effort](https://www.youtube.com/watch?v=hP-VstNdFGo) by [Charles O’Farrell](https://twitter.com/charlesofarrell).

QuickCheck provides a typeclass `CoArbitrary` for generating random _functions_. In this blog post we derive `CoArbitrary` in a standalone setting. For the real definition, see [Test.QuickCheck](https://hackage.haskell.org/package/QuickCheck-2.10.0.1/docs/Test-QuickCheck.html#g:10). Source code for this post is [here](https://github.com/carlohamalainen/playground/tree/master/haskell/coarbitrary).

Some imports:

<pre class="sourceCode haskell"><code class="sourceCode haskell">
&lt;span style="">&gt;&lt;/span> &lt;span style="color: green;">{-# LANGUAGE InstanceSigs #-}&lt;/span>
&lt;span style="">&gt;&lt;/span> &lt;span style="color: green;">{-# LANGUAGE RankNTypes   #-}&lt;/span>
&lt;span style="">&gt;&lt;/span> 
&lt;span style="">&gt;&lt;/span> &lt;span style="color: blue; font-weight: bold;">module&lt;/span> &lt;span style="">Sample&lt;/span> &lt;span style="color: blue; font-weight: bold;">where&lt;/span>
&lt;span style="">&gt;&lt;/span> 
&lt;span style="">&gt;&lt;/span> &lt;span style="color: blue; font-weight: bold;">import&lt;/span> &lt;span style="">Control.Applicative&lt;/span>
&lt;span style="">&gt;&lt;/span> &lt;span style="color: blue; font-weight: bold;">import&lt;/span> &lt;span style="">Control.Monad&lt;/span>
&lt;span style="">&gt;&lt;/span> &lt;span style="color: blue; font-weight: bold;">import&lt;/span> &lt;span style="">System.Random&lt;/span>
</code></pre>

First, a generator of type `Gen a` lets us use a random number generator to get a value of type `a`:

<pre class="sourceCode haskell"><code class="sourceCode haskell">
&lt;span style="">&gt;&lt;/span> &lt;span style="color: blue; font-weight: bold;">newtype&lt;/span> &lt;span style="">Gen&lt;/span> &lt;span style="">a&lt;/span> &lt;span style="color: red;">=&lt;/span> &lt;span style="">Gen&lt;/span>
&lt;span style="">&gt;&lt;/span>     &lt;span style="color: red;">{&lt;/span> &lt;span style="">unGen&lt;/span> &lt;span style="color: red;">::&lt;/span> &lt;span style="">StdGen&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="">a&lt;/span> &lt;span style="color: red;">}&lt;/span>
</code></pre>

(The real `Gen` in QuickCheck is parameterised over the generator, but we don’t need that here. Also, the real `Gen` includes a size, which you can control using [resize](https://hackage.haskell.org/package/QuickCheck-2.11.3/docs/Test-QuickCheck-Gen.html#v:resize) or [scale](https://hackage.haskell.org/package/QuickCheck-2.11.3/docs/Test-QuickCheck-Gen.html#v:scale)).

The `Arbitrary` typeclass:

<pre class="sourceCode haskell"><code class="sourceCode haskell">
&lt;span style="">&gt;&lt;/span> &lt;span style="color: blue; font-weight: bold;">class&lt;/span> &lt;span style="">Arbitrary&lt;/span> &lt;span style="">a&lt;/span> &lt;span style="color: blue; font-weight: bold;">where&lt;/span>
&lt;span style="">&gt;&lt;/span>     &lt;span style="">arbitrary&lt;/span> &lt;span style="color: red;">::&lt;/span> &lt;span style="">Gen&lt;/span> &lt;span style="">a&lt;/span>
</code></pre>

Since `False < True` (there is an instance of `Ord` for `Bool`) we can use [randomR](https://hackage.haskell.org/package/random-1.1/docs/System-Random.html#v:randomR) to define `arbitrary` for `Bool`:

<pre class="sourceCode haskell"><code class="sourceCode haskell">
&lt;span style="">&gt;&lt;/span> &lt;span style="color: blue; font-weight: bold;">instance&lt;/span> &lt;span style="">Arbitrary&lt;/span> &lt;span style="">Bool&lt;/span> &lt;span style="color: blue; font-weight: bold;">where&lt;/span>
&lt;span style="">&gt;&lt;/span>     &lt;span style="">arbitrary&lt;/span> &lt;span style="color: red;">=&lt;/span> &lt;span style="">Gen&lt;/span> &lt;span style="">$&lt;/span> &lt;span style="color: red;">\&lt;/span>&lt;span style="">stdgen&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span>
&lt;span style="">&gt;&lt;/span>       &lt;span style="color: blue; font-weight: bold;">let&lt;/span> &lt;span style="color: red;">(&lt;/span>&lt;span style="">r&lt;/span>&lt;span style="color: red;">,&lt;/span> &lt;span style="color: blue; font-weight: bold;">_&lt;/span>&lt;span style="color: red;">)&lt;/span> &lt;span style="color: red;">=&lt;/span> &lt;span style="">randomR&lt;/span> &lt;span style="color: red;">(&lt;/span>&lt;span style="">False&lt;/span>&lt;span style="color: red;">,&lt;/span> &lt;span style="">True&lt;/span>&lt;span style="color: red;">)&lt;/span> &lt;span style="">stdgen&lt;/span> &lt;span style="color: blue; font-weight: bold;">in&lt;/span> &lt;span style="">r&lt;/span>
</code></pre>

Here’s a first attempt at implementing `Gen (Bool -> Bool)`, a generator for boolean functions:

<pre class="sourceCode haskell"><code class="sourceCode haskell">
&lt;span style="">&gt;&lt;/span> &lt;span style="">genBoolFn0&lt;/span> &lt;span style="color: red;">::&lt;/span> &lt;span style="">Gen&lt;/span> &lt;span style="color: red;">(&lt;/span>&lt;span style="">Bool&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="">Bool&lt;/span>&lt;span style="color: red;">)&lt;/span>
&lt;span style="">&gt;&lt;/span> &lt;span style="">genBoolFn0&lt;/span> &lt;span style="color: red;">=&lt;/span> &lt;span style="">Gen&lt;/span> &lt;span style="">$&lt;/span> &lt;span style="color: red;">\&lt;/span>&lt;span style="">stdgen&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span>
&lt;span style="">&gt;&lt;/span>   &lt;span style="color: red;">\&lt;/span>&lt;span style="">a&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="color: blue; font-weight: bold;">let&lt;/span> &lt;span style="color: red;">(&lt;/span>&lt;span style="">r&lt;/span>&lt;span style="color: red;">,&lt;/span> &lt;span style="color: blue; font-weight: bold;">_&lt;/span>&lt;span style="color: red;">)&lt;/span> &lt;span style="color: red;">=&lt;/span> &lt;span style="">randomR&lt;/span> &lt;span style="color: red;">(&lt;/span>&lt;span style="">False&lt;/span>&lt;span style="color: red;">,&lt;/span> &lt;span style="">True&lt;/span>&lt;span style="color: red;">)&lt;/span> &lt;span style="">stdgen&lt;/span> &lt;span style="color: blue; font-weight: bold;">in&lt;/span> &lt;span style="">r&lt;/span>
</code></pre>

The type is right but it’s going to generate pretty boring functions since it doesn’t even use the `a`:

<pre class="sourceCode haskell"><code class="sourceCode haskell">
&lt;span style="">&gt;&lt;/span> &lt;span style="">runBoolFnGen&lt;/span> &lt;span style="color: red;">::&lt;/span> &lt;span style="">Gen&lt;/span> &lt;span style="color: red;">(&lt;/span>&lt;span style="">Bool&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="">Bool&lt;/span>&lt;span style="color: red;">)&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="">IO&lt;/span> &lt;span style="">()&lt;/span>
&lt;span style="">&gt;&lt;/span> &lt;span style="">runBoolFnGen&lt;/span> &lt;span style="">g&lt;/span> &lt;span style="color: red;">=&lt;/span> &lt;span style="color: blue; font-weight: bold;">do&lt;/span>
&lt;span style="">&gt;&lt;/span>   &lt;span style="">fns&lt;/span>  &lt;span style="color: red;">&lt;-&lt;/span> &lt;span style="">samples&lt;/span> &lt;span style="">g&lt;/span>
&lt;span style="">&gt;&lt;/span> 
&lt;span style="">&gt;&lt;/span>   &lt;span style="">forM_&lt;/span> &lt;span style="">fns&lt;/span>  &lt;span style="">$&lt;/span> &lt;span style="color: red;">\&lt;/span>&lt;span style="">f&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="color: blue; font-weight: bold;">do&lt;/span>
&lt;span style="">&gt;&lt;/span>     &lt;span style="">putStrLn&lt;/span> &lt;span style="">$&lt;/span> &lt;span style="color: teal;">"True  =&gt; "&lt;/span> &lt;span style="">++&lt;/span> &lt;span style="">show&lt;/span> &lt;span style="color: red;">(&lt;/span>&lt;span style="">f&lt;/span> &lt;span style="">True&lt;/span>&lt;span style="color: red;">)&lt;/span>
&lt;span style="">&gt;&lt;/span>     &lt;span style="">putStrLn&lt;/span> &lt;span style="">$&lt;/span> &lt;span style="color: teal;">"False =&gt; "&lt;/span> &lt;span style="">++&lt;/span> &lt;span style="">show&lt;/span> &lt;span style="color: red;">(&lt;/span>&lt;span style="">f&lt;/span> &lt;span style="">False&lt;/span>&lt;span style="color: red;">)&lt;/span>
&lt;span style="">&gt;&lt;/span>     &lt;span style="">putStrLn&lt;/span> &lt;span style="color: teal;">""&lt;/span>
</code></pre>

The functions are either `const True` or `const False`. Not useful.

    
    ghci> runBoolFnGen genBoolFn0 
    True  => True
    False => True
    
    True  => False
    False => False
    
    True  => False
    False => False
    
    True  => True
    False => True
    
    True  => False
    False => False
    
    True  => True
    False => True
    
    True  => False
    False => False
    
    True  => False
    False => False
    
    True  => False
    False => False

We need to split on the `a` somehow:

<pre class="sourceCode haskell"><code class="sourceCode haskell">
&lt;span style="">&gt;&lt;/span> &lt;span style="">genBoolFn1&lt;/span> &lt;span style="color: red;">::&lt;/span> &lt;span style="">Gen&lt;/span> &lt;span style="color: red;">(&lt;/span>&lt;span style="">Bool&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="">Bool&lt;/span>&lt;span style="color: red;">)&lt;/span>
&lt;span style="">&gt;&lt;/span> &lt;span style="">genBoolFn1&lt;/span> &lt;span style="color: red;">=&lt;/span> &lt;span style="">Gen&lt;/span> &lt;span style="">$&lt;/span> &lt;span style="color: red;">\&lt;/span>&lt;span style="">stdgen&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="color: red;">\&lt;/span>&lt;span style="">a&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="color: blue; font-weight: bold;">case&lt;/span> &lt;span style="">a&lt;/span> &lt;span style="color: blue; font-weight: bold;">of&lt;/span>
&lt;span style="">&gt;&lt;/span>   &lt;span style="">True&lt;/span>  &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="color: blue; font-weight: bold;">let&lt;/span> &lt;span style="color: red;">(&lt;/span>&lt;span style="">r&lt;/span>&lt;span style="color: red;">,&lt;/span> &lt;span style="color: blue; font-weight: bold;">_&lt;/span>&lt;span style="color: red;">)&lt;/span> &lt;span style="color: red;">=&lt;/span> &lt;span style="">randomR&lt;/span> &lt;span style="color: red;">(&lt;/span>&lt;span style="">False&lt;/span>&lt;span style="color: red;">,&lt;/span> &lt;span style="">True&lt;/span>&lt;span style="color: red;">)&lt;/span> &lt;span style="">stdgen&lt;/span> &lt;span style="color: blue; font-weight: bold;">in&lt;/span> &lt;span style="">r&lt;/span>
&lt;span style="">&gt;&lt;/span>   &lt;span style="">False&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="color: blue; font-weight: bold;">let&lt;/span> &lt;span style="color: red;">(&lt;/span>&lt;span style="">r&lt;/span>&lt;span style="color: red;">,&lt;/span> &lt;span style="color: blue; font-weight: bold;">_&lt;/span>&lt;span style="color: red;">)&lt;/span> &lt;span style="color: red;">=&lt;/span> &lt;span style="">randomR&lt;/span> &lt;span style="color: red;">(&lt;/span>&lt;span style="">False&lt;/span>&lt;span style="color: red;">,&lt;/span> &lt;span style="">True&lt;/span>&lt;span style="color: red;">)&lt;/span> &lt;span style="">stdgen&lt;/span> &lt;span style="color: blue; font-weight: bold;">in&lt;/span> &lt;span style="">r&lt;/span>
</code></pre>

This isn’t any better. The other thing we can change is the generator. Fortunately, `StdGen` is an instance of `RandomGen`, so we have the [split](https://hackage.haskell.org/package/random-1.1/docs/System-Random.html#v:split) function:

<pre class="sourceCode haskell"><code class="sourceCode haskell">
&lt;span style="">&gt;&lt;/span> &lt;span style="color: green;">-- The split operation allows one to obtain two&lt;/span>
&lt;span style="">&gt;&lt;/span> &lt;span style="color: green;">-- distinct random number generators. This is&lt;/span>
&lt;span style="">&gt;&lt;/span> &lt;span style="color: green;">-- very useful in functional programs (for example,&lt;/span>
&lt;span style="">&gt;&lt;/span> &lt;span style="color: green;">-- when passing a random number generator down to&lt;/span>
&lt;span style="">&gt;&lt;/span> &lt;span style="color: green;">-- recursive calls), but very little work has been done&lt;/span>
&lt;span style="">&gt;&lt;/span> &lt;span style="color: green;">-- on statistically robust implementations of split&lt;/span>
&lt;span style="">&gt;&lt;/span> &lt;span style="color: green;">-- ([System.Random, System.Random] are the only&lt;/span>
&lt;span style="">&gt;&lt;/span> &lt;span style="color: green;">-- examples we know of).&lt;/span>
&lt;span style="">&gt;&lt;/span> 
&lt;span style="">&gt;&lt;/span> &lt;span style="">split&lt;/span> &lt;span style="color: red;">::&lt;/span> &lt;span style="">StdGen&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="color: red;">(&lt;/span>&lt;span style="">StdGen&lt;/span>&lt;span style="color: red;">,&lt;/span> &lt;span style="">StdGen&lt;/span>&lt;span style="color: red;">)&lt;/span>
&lt;span style="">&gt;&lt;/span> &lt;span style="">split&lt;/span> &lt;span style="color: red;">=&lt;/span> &lt;span style="">...&lt;/span>
</code></pre>

Taking advantage of laziness, we can use split to write a pure function that gives us an infinite sequence of statistically distinct generators:

<pre class="sourceCode haskell"><code class="sourceCode haskell">
&lt;span style="">&gt;&lt;/span> &lt;span style="">splitGenerator&lt;/span> &lt;span style="color: red;">::&lt;/span> &lt;span style="">RandomGen&lt;/span> &lt;span style="">a&lt;/span> &lt;span style="color: red;">=&gt;&lt;/span> &lt;span style="">a&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="color: red;">[&lt;/span>&lt;span style="">a&lt;/span>&lt;span style="color: red;">]&lt;/span>
&lt;span style="">&gt;&lt;/span> &lt;span style="">splitGenerator&lt;/span> &lt;span style="">r&lt;/span> &lt;span style="color: red;">=&lt;/span> &lt;span style="">r0&lt;/span> &lt;span style="">:&lt;/span> &lt;span style="">splitGenerator&lt;/span> &lt;span style="">r1&lt;/span>
&lt;span style="">&gt;&lt;/span>   &lt;span style="color: blue; font-weight: bold;">where&lt;/span>
&lt;span style="">&gt;&lt;/span>     &lt;span style="color: red;">(&lt;/span>&lt;span style="">r0&lt;/span>&lt;span style="color: red;">,&lt;/span> &lt;span style="">r1&lt;/span>&lt;span style="color: red;">)&lt;/span> &lt;span style="color: red;">=&lt;/span> &lt;span style="">split&lt;/span> &lt;span style="">r&lt;/span>
</code></pre>

This is exactly what we need to permute the generator in `genBoolFn1`. Let’s map `True` to the generator at index `` and `False` to to the generator at index `1`:

<pre class="sourceCode haskell"><code class="sourceCode haskell">
&lt;span style="">&gt;&lt;/span> &lt;span style="">genBoolFn2&lt;/span> &lt;span style="color: red;">::&lt;/span> &lt;span style="">Gen&lt;/span> &lt;span style="color: red;">(&lt;/span>&lt;span style="">Bool&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="">Bool&lt;/span>&lt;span style="color: red;">)&lt;/span>
&lt;span style="">&gt;&lt;/span> &lt;span style="">genBoolFn2&lt;/span> &lt;span style="color: red;">=&lt;/span> &lt;span style="">Gen&lt;/span> &lt;span style="">$&lt;/span> &lt;span style="color: red;">\&lt;/span>&lt;span style="">stdgen&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="color: red;">\&lt;/span>&lt;span style="">a&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="color: blue; font-weight: bold;">case&lt;/span> &lt;span style="">a&lt;/span> &lt;span style="color: blue; font-weight: bold;">of&lt;/span>
&lt;span style="">&gt;&lt;/span>   &lt;span style="">True&lt;/span>  &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="color: blue; font-weight: bold;">let&lt;/span> &lt;span style="color: red;">(&lt;/span>&lt;span style="">r&lt;/span>&lt;span style="color: red;">,&lt;/span> &lt;span style="color: blue; font-weight: bold;">_&lt;/span>&lt;span style="color: red;">)&lt;/span> &lt;span style="color: red;">=&lt;/span> &lt;span style="">randomR&lt;/span>
&lt;span style="">&gt;&lt;/span>                           &lt;span style="color: red;">(&lt;/span>&lt;span style="">False&lt;/span>&lt;span style="color: red;">,&lt;/span> &lt;span style="">True&lt;/span>&lt;span style="color: red;">)&lt;/span>
&lt;span style="">&gt;&lt;/span>                           &lt;span style="color: red;">(&lt;/span>&lt;span style="">splitGenerator&lt;/span> &lt;span style="">stdgen&lt;/span> &lt;span style="">!!&lt;/span> &lt;span class="hs-num">0&lt;/span>&lt;span style="color: red;">)&lt;/span>
&lt;span style="">&gt;&lt;/span>             &lt;span style="color: blue; font-weight: bold;">in&lt;/span> &lt;span style="">r&lt;/span>
&lt;span style="">&gt;&lt;/span>   &lt;span style="">False&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="color: blue; font-weight: bold;">let&lt;/span> &lt;span style="color: red;">(&lt;/span>&lt;span style="">r&lt;/span>&lt;span style="color: red;">,&lt;/span> &lt;span style="color: blue; font-weight: bold;">_&lt;/span>&lt;span style="color: red;">)&lt;/span> &lt;span style="color: red;">=&lt;/span> &lt;span style="">randomR&lt;/span>
&lt;span style="">&gt;&lt;/span>                           &lt;span style="color: red;">(&lt;/span>&lt;span style="">False&lt;/span>&lt;span style="color: red;">,&lt;/span> &lt;span style="">True&lt;/span>&lt;span style="color: red;">)&lt;/span>
&lt;span style="">&gt;&lt;/span>                           &lt;span style="color: red;">(&lt;/span>&lt;span style="">splitGenerator&lt;/span> &lt;span style="">stdgen&lt;/span> &lt;span style="">!!&lt;/span> &lt;span class="hs-num">1&lt;/span>&lt;span style="color: red;">)&lt;/span>
&lt;span style="">&gt;&lt;/span>             &lt;span style="color: blue; font-weight: bold;">in&lt;/span> &lt;span style="">r&lt;/span>
</code></pre>

Now the random functions look more random:

    
    ghci> runBoolFnGen genBoolFn2
    True  => False
    False => True
    
    True  => True
    False => False
    
    True  => False
    False => True
    
    True  => True
    False => False
    
    True  => True
    False => True
    
    True  => True
    False => False
    
    True  => True
    False => True
    
    True  => False
    False => False
    
    True  => False
    False => True

So, what about random integer functions `Int -> Int`? We need to map any integer to one of the split generators, in other words we need a mapping $\mathbb{N} \rightarrow \mathbb Z_{\ge 0}$. Send the non-negative integers to the non-negative even integers, and the negative integers to the positive odd integers:

\[  
n \rightarrow  
\begin{cases}  
2n & \mbox{if } n \geq 0 \\  
2(-n) + 1 & \mbox{if } n < 0 \end{cases} \] 

In Haskell this looks like:

<pre class="sourceCode haskell"><code class="sourceCode haskell">
&lt;span style="">&gt;&lt;/span> &lt;span style="color: red;">\&lt;/span>&lt;span style="">n&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="">variant&lt;/span> &lt;span style="">$&lt;/span> &lt;span style="color: blue; font-weight: bold;">if&lt;/span> &lt;span style="">n&lt;/span> &lt;span style="">&gt;=&lt;/span> &lt;span class="hs-num">0&lt;/span> &lt;span style="color: blue; font-weight: bold;">then&lt;/span> &lt;span class="hs-num">2&lt;/span>&lt;span style="">*&lt;/span>&lt;span style="">n&lt;/span> &lt;span style="color: blue; font-weight: bold;">else&lt;/span> &lt;span class="hs-num">2&lt;/span>&lt;span style="">*&lt;/span>&lt;span style="color: red;">(&lt;/span>&lt;span style="color: green;">-&lt;/span>&lt;span style="">n&lt;/span>&lt;span style="color: red;">)&lt;/span> &lt;span style="">+&lt;/span> &lt;span class="hs-num">1&lt;/span>
</code></pre>

where

<pre class="sourceCode haskell"><code class="sourceCode haskell">
&lt;span style="">&gt;&lt;/span> &lt;span style="">variant&lt;/span> &lt;span style="color: red;">::&lt;/span> &lt;span style="">Int&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="">Gen&lt;/span> &lt;span style="">b&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="">Gen&lt;/span> &lt;span style="">b&lt;/span>
&lt;span style="">&gt;&lt;/span> &lt;span style="">variant&lt;/span> &lt;span style="">v&lt;/span> &lt;span style="color: red;">(&lt;/span>&lt;span style="">Gen&lt;/span> &lt;span style="">g&lt;/span>&lt;span style="color: red;">)&lt;/span> &lt;span style="color: red;">=&lt;/span> &lt;span style="">Gen&lt;/span> &lt;span style="">$&lt;/span> &lt;span style="color: red;">\&lt;/span>&lt;span style="">r&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="">g&lt;/span> &lt;span style="">$&lt;/span> &lt;span style="">splitGenerator&lt;/span> &lt;span style="">r&lt;/span> &lt;span style="">!!&lt;/span> &lt;span style="color: red;">(&lt;/span>&lt;span style="">v&lt;/span>&lt;span style="">+&lt;/span>&lt;span class="hs-num">1&lt;/span>&lt;span style="color: red;">)&lt;/span>
</code></pre>

Capture this with a typeclass:

<pre class="sourceCode haskell"><code class="sourceCode haskell">
&lt;span style="">&gt;&lt;/span> &lt;span style="color: blue; font-weight: bold;">class&lt;/span> &lt;span style="">CoArbitrary&lt;/span> &lt;span style="">a&lt;/span> &lt;span style="color: blue; font-weight: bold;">where&lt;/span>
&lt;span style="">&gt;&lt;/span>     &lt;span style="">coarbitrary&lt;/span> &lt;span style="color: red;">::&lt;/span> &lt;span style="">a&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="">Gen&lt;/span> &lt;span style="">b&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="">Gen&lt;/span> &lt;span style="">b&lt;/span>
&lt;span style="">&gt;&lt;/span> 
&lt;span style="">&gt;&lt;/span> &lt;span style="color: blue; font-weight: bold;">instance&lt;/span> &lt;span style="">CoArbitrary&lt;/span> &lt;span style="">Bool&lt;/span> &lt;span style="color: blue; font-weight: bold;">where&lt;/span>
&lt;span style="">&gt;&lt;/span>   &lt;span style="">coarbitrary&lt;/span> &lt;span style="">False&lt;/span> &lt;span style="color: red;">=&lt;/span> &lt;span style="">variant&lt;/span> &lt;span class="hs-num">0&lt;/span>
&lt;span style="">&gt;&lt;/span>   &lt;span style="">coarbitrary&lt;/span> &lt;span style="">True&lt;/span>  &lt;span style="color: red;">=&lt;/span> &lt;span style="">variant&lt;/span> &lt;span class="hs-num">1&lt;/span>
&lt;span style="">&gt;&lt;/span> 
&lt;span style="">&gt;&lt;/span> &lt;span style="color: blue; font-weight: bold;">instance&lt;/span> &lt;span style="">CoArbitrary&lt;/span> &lt;span style="">Int&lt;/span> &lt;span style="color: blue; font-weight: bold;">where&lt;/span>
&lt;span style="">&gt;&lt;/span>     &lt;span style="">coarbitrary&lt;/span> &lt;span style="">n&lt;/span> &lt;span style="color: red;">=&lt;/span> &lt;span style="">variant&lt;/span> &lt;span style="">$&lt;/span> &lt;span style="color: blue; font-weight: bold;">if&lt;/span> &lt;span style="">n&lt;/span> &lt;span style="">&gt;=&lt;/span> &lt;span class="hs-num">0&lt;/span> &lt;span style="color: blue; font-weight: bold;">then&lt;/span> &lt;span class="hs-num">2&lt;/span>&lt;span style="">*&lt;/span>&lt;span style="">n&lt;/span> &lt;span style="color: blue; font-weight: bold;">else&lt;/span> &lt;span class="hs-num">2&lt;/span>&lt;span style="">*&lt;/span>&lt;span style="color: red;">(&lt;/span>&lt;span style="color: green;">-&lt;/span>&lt;span style="">n&lt;/span>&lt;span style="color: red;">)&lt;/span> &lt;span style="">+&lt;/span> &lt;span class="hs-num">1&lt;/span>
</code></pre>

With some experimentation we can extend the `CoArbitrary` definitions to lists and tuples:

<pre class="sourceCode haskell"><code class="sourceCode haskell">
&lt;span style="">&gt;&lt;/span> &lt;span style="color: blue; font-weight: bold;">instance&lt;/span> &lt;span style="">CoArbitrary&lt;/span> &lt;span style="">a&lt;/span> &lt;span style="color: red;">=&gt;&lt;/span> &lt;span style="">CoArbitrary&lt;/span> &lt;span style="color: red;">[&lt;/span>&lt;span style="">a&lt;/span>&lt;span style="color: red;">]&lt;/span> &lt;span style="color: blue; font-weight: bold;">where&lt;/span>
&lt;span style="">&gt;&lt;/span>   &lt;span style="">coarbitrary&lt;/span> &lt;span style="">[]&lt;/span>     &lt;span style="color: red;">=&lt;/span> &lt;span style="">variant&lt;/span> &lt;span class="hs-num">0&lt;/span>
&lt;span style="">&gt;&lt;/span>   &lt;span style="">coarbitrary&lt;/span> &lt;span style="color: red;">(&lt;/span>&lt;span style="">x&lt;/span>&lt;span style="">:&lt;/span>&lt;span style="">xs&lt;/span>&lt;span style="color: red;">)&lt;/span> &lt;span style="color: red;">=&lt;/span> &lt;span style="">variant&lt;/span> &lt;span class="hs-num">1&lt;/span> &lt;span style="">.&lt;/span> &lt;span style="">coarbitrary&lt;/span> &lt;span style="color: red;">(&lt;/span>&lt;span style="">x&lt;/span>&lt;span style="color: red;">,&lt;/span> &lt;span style="">xs&lt;/span>&lt;span style="color: red;">)&lt;/span>
&lt;span style="">&gt;&lt;/span> 
&lt;span style="">&gt;&lt;/span> &lt;span style="color: blue; font-weight: bold;">instance&lt;/span> &lt;span style="color: red;">(&lt;/span>&lt;span style="">CoArbitrary&lt;/span> &lt;span style="">a&lt;/span>&lt;span style="color: red;">,&lt;/span> &lt;span style="">CoArbitrary&lt;/span> &lt;span style="">b&lt;/span>&lt;span style="color: red;">)&lt;/span> &lt;span style="color: red;">=&gt;&lt;/span> &lt;span style="">CoArbitrary&lt;/span> &lt;span style="color: red;">(&lt;/span>&lt;span style="">a&lt;/span>&lt;span style="color: red;">,&lt;/span> &lt;span style="">b&lt;/span>&lt;span style="color: red;">)&lt;/span> &lt;span style="color: blue; font-weight: bold;">where&lt;/span>
&lt;span style="">&gt;&lt;/span>   &lt;span style="">coarbitrary&lt;/span> &lt;span style="color: red;">(&lt;/span>&lt;span style="">x&lt;/span>&lt;span style="color: red;">,&lt;/span> &lt;span style="">y&lt;/span>&lt;span style="color: red;">)&lt;/span> &lt;span style="color: red;">=&lt;/span> &lt;span style="">coarbitrary&lt;/span> &lt;span style="">x&lt;/span> &lt;span style="">.&lt;/span> &lt;span style="">coarbitrary&lt;/span> &lt;span style="">y&lt;/span>
</code></pre>

In general, if we have `CoArbitrary a` and `Arbitrary b` then we can derive `Arbitrary (a -> b)`:

<pre class="sourceCode haskell"><code class="sourceCode haskell">
&lt;span style="">&gt;&lt;/span> &lt;span style="color: blue; font-weight: bold;">instance&lt;/span> &lt;span style="color: red;">(&lt;/span>&lt;span style="">CoArbitrary&lt;/span> &lt;span style="">a&lt;/span>&lt;span style="color: red;">,&lt;/span> &lt;span style="">Arbitrary&lt;/span> &lt;span style="">b&lt;/span>&lt;span style="color: red;">)&lt;/span> &lt;span style="color: red;">=&gt;&lt;/span> &lt;span style="">Arbitrary&lt;/span> &lt;span style="color: red;">(&lt;/span>&lt;span style="">a&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="">b&lt;/span>&lt;span style="color: red;">)&lt;/span> &lt;span style="color: blue; font-weight: bold;">where&lt;/span>
&lt;span style="">&gt;&lt;/span>     &lt;span style="">arbitrary&lt;/span> &lt;span style="color: red;">=&lt;/span> &lt;span style="">promote&lt;/span> &lt;span style="color: red;">(&lt;/span>&lt;span style="color: red;">\&lt;/span>&lt;span style="">a&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="">coarbitrary&lt;/span> &lt;span style="">a&lt;/span> &lt;span style="">arbitrary&lt;/span>&lt;span style="color: red;">)&lt;/span>
&lt;span style="">&gt;&lt;/span> 
&lt;span style="">&gt;&lt;/span> &lt;span style="">promote&lt;/span> &lt;span style="color: red;">::&lt;/span> &lt;span style="color: red;">(&lt;/span>&lt;span style="">a&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="">Gen&lt;/span> &lt;span style="">b&lt;/span>&lt;span style="color: red;">)&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="color: red;">(&lt;/span>&lt;span style="">Gen&lt;/span> &lt;span style="color: red;">(&lt;/span>&lt;span style="">a&lt;/span>&lt;span style="color: red;">-&gt;&lt;/span>&lt;span style="">b&lt;/span>&lt;span style="color: red;">)&lt;/span>&lt;span style="color: red;">)&lt;/span>
&lt;span style="">&gt;&lt;/span> &lt;span style="">promote&lt;/span> &lt;span style="">f&lt;/span> &lt;span style="color: red;">=&lt;/span> &lt;span style="">Gen&lt;/span> &lt;span style="">$&lt;/span> &lt;span style="color: red;">\&lt;/span>&lt;span style="">r&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span>
&lt;span style="">&gt;&lt;/span>   &lt;span style="color: red;">\&lt;/span>&lt;span style="">a&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="color: blue; font-weight: bold;">let&lt;/span> &lt;span style="">Gen&lt;/span> &lt;span style="">h&lt;/span> &lt;span style="color: red;">=&lt;/span> &lt;span style="">f&lt;/span> &lt;span style="">a&lt;/span>
&lt;span style="">&gt;&lt;/span>          &lt;span style="color: blue; font-weight: bold;">in&lt;/span> &lt;span style="">h&lt;/span> &lt;span style="">r&lt;/span>
</code></pre>

Here are more examples of random functions:

<pre class="sourceCode haskell"><code class="sourceCode haskell">
&lt;span style="">&gt;&lt;/span> &lt;span style="">runGenFn&lt;/span> &lt;span style="color: red;">::&lt;/span> &lt;span style="color: red;">(&lt;/span>&lt;span style="">Arbitrary&lt;/span> &lt;span style="">a&lt;/span>&lt;span style="color: red;">,&lt;/span> &lt;span style="">Arbitrary&lt;/span> &lt;span style="">b&lt;/span>&lt;span style="color: red;">,&lt;/span> &lt;span style="">Show&lt;/span> &lt;span style="">a&lt;/span>&lt;span style="color: red;">,&lt;/span> &lt;span style="">Show&lt;/span> &lt;span style="">b&lt;/span>&lt;span style="color: red;">)&lt;/span>
&lt;span style="">&gt;&lt;/span>          &lt;span style="color: red;">=&gt;&lt;/span> &lt;span style="">Gen&lt;/span> &lt;span style="color: red;">(&lt;/span>&lt;span style="">a&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="">b&lt;/span>&lt;span style="color: red;">)&lt;/span>
&lt;span style="">&gt;&lt;/span>          &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="color: red;">[&lt;/span>&lt;span style="">a&lt;/span>&lt;span style="color: red;">]&lt;/span>
&lt;span style="">&gt;&lt;/span>          &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="">IO&lt;/span> &lt;span style="">()&lt;/span>
&lt;span style="">&gt;&lt;/span> &lt;span style="">runGenFn&lt;/span> &lt;span style="">g&lt;/span> &lt;span style="color: blue; font-weight: bold;">as&lt;/span> &lt;span style="color: red;">=&lt;/span> &lt;span style="color: blue; font-weight: bold;">do&lt;/span>
&lt;span style="">&gt;&lt;/span>   &lt;span style="">fns&lt;/span>  &lt;span style="color: red;">&lt;-&lt;/span> &lt;span style="">samples&lt;/span> &lt;span style="">g&lt;/span>
&lt;span style="">&gt;&lt;/span> 
&lt;span style="">&gt;&lt;/span>   &lt;span style="">forM_&lt;/span> &lt;span style="">fns&lt;/span>  &lt;span style="">$&lt;/span> &lt;span style="color: red;">\&lt;/span>&lt;span style="">f&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="color: blue; font-weight: bold;">do&lt;/span>
&lt;span style="">&gt;&lt;/span>     &lt;span style="">forM_&lt;/span> &lt;span style="color: blue; font-weight: bold;">as&lt;/span> &lt;span style="">$&lt;/span> &lt;span style="color: red;">\&lt;/span>&lt;span style="">a&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="">putStrLn&lt;/span> &lt;span style="">$&lt;/span> &lt;span style="">show&lt;/span> &lt;span style="">a&lt;/span> &lt;span style="">++&lt;/span> &lt;span style="color: teal;">" =&gt; "&lt;/span>
&lt;span style="">&gt;&lt;/span>                                        &lt;span style="">++&lt;/span> &lt;span style="">show&lt;/span> &lt;span style="color: red;">(&lt;/span>&lt;span style="">f&lt;/span> &lt;span style="">a&lt;/span>&lt;span style="color: red;">)&lt;/span>
&lt;span style="">&gt;&lt;/span>     &lt;span style="">putStrLn&lt;/span> &lt;span style="color: teal;">""&lt;/span>
</code></pre>

Running in ghci:

    
    ghci> runGenFn (arbitrary :: Gen (Int -> Int)) [0, 1, 2]
    0 => 198
    1 => 940
    2 => -200
    
    0 => 734
    1 => -627
    2 => 6
    
    0 => 965
    1 => 581
    2 => -585
    
    0 => -306
    1 => -918
    2 => 678
    
    0 => -929
    1 => 336
    2 => -696
    
    0 => -66
    1 => 123
    2 => 875
    
    0 => -234
    1 => -673
    2 => 216
    
    0 => 355
    1 => 56
    2 => -615
    
    0 => 278
    1 => 116
    2 => 967
    
    ghci> runGenFn (arbitrary :: Gen (Int -> Bool)) [0, 1, 2]
    0 => False
    1 => True
    2 => False
    
    0 => True
    1 => False
    2 => True
    
    0 => True
    1 => False
    2 => False
    
    0 => True
    1 => False
    2 => False
    
    0 => True
    1 => True
    2 => True
    
    0 => True
    1 => True
    2 => False
    
    0 => False
    1 => True
    2 => False
    
    0 => True
    1 => True
    2 => False
    
    0 => True
    1 => True
    2 => True
    
    ghci> runGenFn (arbitrary :: Gen ([Int] -> [Int])) [[42], [0, 1, 2]]
    [42] => [-93,-540,-715,-557,-249]
    [0,1,2] => [433,97,665,554,-690,635]
    
    [42] => [-785,174,-676,-662,-549]
    [0,1,2] => [-735,-328,226,-524,423]
    
    [42] => [157,976,-774]
    [0,1,2] => [-197,608,-520,-37,-689]
    
    [42] => [-684]
    [0,1,2] => [902,-138,-33,689,-774,-713,474,-638]
    
    [42] => [-782,540,649,320,-326,92,896,-76]
    [0,1,2] => [156]
    
    [42] => [524,137]
    [0,1,2] => [642,-763,771,-400,825,71,895,586,252,37]
    
    [42] => [641,-304,-890,-375,449,-608,662,546,-740,-406]
    [0,1,2] => [-632,-685,-232,202,-994,666,-121]
    
    [42] => [200,599,-844]
    [0,1,2] => [-554,149,370,547,-755,-706,131]
    
    [42] => [-898,645,-472]
    [0,1,2] => [-77]

## Appendix

To get the code above to work we need instances for `Monad`, `Functor`, and `Applicative`. These are all lifted from [Test.QuickCheck](https://hackage.haskell.org/package/QuickCheck-2.10.0.1/docs/Test-QuickCheck.html).

<pre class="sourceCode haskell"><code class="sourceCode haskell">
&lt;span style="">&gt;&lt;/span> &lt;span style="color: blue; font-weight: bold;">instance&lt;/span> &lt;span style="">Monad&lt;/span> &lt;span style="">Gen&lt;/span> &lt;span style="color: blue; font-weight: bold;">where&lt;/span>
&lt;span style="">&gt;&lt;/span>     &lt;span style="">return&lt;/span> &lt;span style="color: red;">::&lt;/span> &lt;span style="">a&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="">Gen&lt;/span> &lt;span style="">a&lt;/span>
&lt;span style="">&gt;&lt;/span>     &lt;span style="">return&lt;/span> &lt;span style="">a&lt;/span> &lt;span style="color: red;">=&lt;/span> &lt;span style="">Gen&lt;/span> &lt;span style="">$&lt;/span> &lt;span style="color: red;">\&lt;/span>&lt;span style="color: blue; font-weight: bold;">_&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="">a&lt;/span>
&lt;span style="">&gt;&lt;/span> 
&lt;span style="">&gt;&lt;/span>     &lt;span style="color: red;">(&lt;/span>&lt;span style="">&gt;&gt;=&lt;/span>&lt;span style="color: red;">)&lt;/span> &lt;span style="color: red;">::&lt;/span> &lt;span style="">Gen&lt;/span> &lt;span style="">a&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="color: red;">(&lt;/span>&lt;span style="">a&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="">Gen&lt;/span> &lt;span style="">b&lt;/span>&lt;span style="color: red;">)&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="">Gen&lt;/span> &lt;span style="">b&lt;/span>
&lt;span style="">&gt;&lt;/span> 
&lt;span style="">&gt;&lt;/span>     &lt;span style="color: red;">(&lt;/span>&lt;span style="">Gen&lt;/span> &lt;span style="">g&lt;/span>&lt;span style="color: red;">)&lt;/span> &lt;span style="">&gt;&gt;=&lt;/span> &lt;span style="">f&lt;/span> &lt;span style="color: red;">=&lt;/span> &lt;span style="">Gen&lt;/span> &lt;span style="">$&lt;/span> &lt;span style="color: red;">\&lt;/span>&lt;span style="">r&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span>
&lt;span style="">&gt;&lt;/span>        &lt;span style="color: blue; font-weight: bold;">let&lt;/span> &lt;span style="color: red;">(&lt;/span>&lt;span style="">r1&lt;/span>&lt;span style="color: red;">,&lt;/span> &lt;span style="">r2&lt;/span>&lt;span style="color: red;">)&lt;/span> &lt;span style="color: red;">=&lt;/span> &lt;span style="">split&lt;/span> &lt;span style="">r&lt;/span>
&lt;span style="">&gt;&lt;/span>            &lt;span style="">Gen&lt;/span> &lt;span style="">k&lt;/span> &lt;span style="color: red;">=&lt;/span> &lt;span style="">f&lt;/span> &lt;span style="">$&lt;/span> &lt;span style="">g&lt;/span> &lt;span style="">r1&lt;/span>
&lt;span style="">&gt;&lt;/span>         &lt;span style="color: blue; font-weight: bold;">in&lt;/span> &lt;span style="">k&lt;/span> &lt;span style="">r2&lt;/span>
&lt;span style="">&gt;&lt;/span> 
&lt;span style="">&gt;&lt;/span> &lt;span style="color: blue; font-weight: bold;">instance&lt;/span> &lt;span style="">Functor&lt;/span> &lt;span style="">Gen&lt;/span> &lt;span style="color: blue; font-weight: bold;">where&lt;/span>
&lt;span style="">&gt;&lt;/span>     &lt;span style="">fmap&lt;/span> &lt;span style="">f&lt;/span> &lt;span style="color: red;">(&lt;/span>&lt;span style="">Gen&lt;/span> &lt;span style="">h&lt;/span>&lt;span style="color: red;">)&lt;/span> &lt;span style="color: red;">=&lt;/span> &lt;span style="">Gen&lt;/span> &lt;span style="">$&lt;/span> &lt;span style="color: red;">\&lt;/span>&lt;span style="">r&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="">f&lt;/span> &lt;span style="color: red;">(&lt;/span>&lt;span style="">h&lt;/span> &lt;span style="">r&lt;/span>&lt;span style="color: red;">)&lt;/span>
&lt;span style="">&gt;&lt;/span> 
&lt;span style="">&gt;&lt;/span> &lt;span style="color: blue; font-weight: bold;">instance&lt;/span> &lt;span style="">Applicative&lt;/span> &lt;span style="">Gen&lt;/span> &lt;span style="color: blue; font-weight: bold;">where&lt;/span>
&lt;span style="">&gt;&lt;/span>     &lt;span style="">pure&lt;/span> &lt;span style="">x&lt;/span> &lt;span style="color: red;">=&lt;/span> &lt;span style="">Gen&lt;/span> &lt;span style="">$&lt;/span> &lt;span style="color: red;">\&lt;/span>&lt;span style="color: blue; font-weight: bold;">_&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="">x&lt;/span>
&lt;span style="">&gt;&lt;/span> 
&lt;span style="">&gt;&lt;/span>     &lt;span style="">f&lt;/span> &lt;span style="">&lt;*&gt;&lt;/span> &lt;span style="">x&lt;/span> &lt;span style="color: red;">=&lt;/span> &lt;span style="color: blue; font-weight: bold;">do&lt;/span>
&lt;span style="">&gt;&lt;/span>         &lt;span style="">f'&lt;/span> &lt;span style="color: red;">&lt;-&lt;/span> &lt;span style="">f&lt;/span>
&lt;span style="">&gt;&lt;/span>         &lt;span style="">x'&lt;/span> &lt;span style="color: red;">&lt;-&lt;/span> &lt;span style="">x&lt;/span>
&lt;span style="">&gt;&lt;/span>         &lt;span style="">return&lt;/span> &lt;span style="">$&lt;/span> &lt;span style="">f'&lt;/span> &lt;span style="">x'&lt;/span>
&lt;span style="">&gt;&lt;/span> 
&lt;span style="">&gt;&lt;/span> &lt;span style="">generate&lt;/span> &lt;span style="color: red;">::&lt;/span> &lt;span style="">Gen&lt;/span> &lt;span style="">a&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="">IO&lt;/span> &lt;span style="">a&lt;/span>
&lt;span style="">&gt;&lt;/span> &lt;span style="">generate&lt;/span> &lt;span style="color: red;">(&lt;/span>&lt;span style="">Gen&lt;/span> &lt;span style="">g&lt;/span>&lt;span style="color: red;">)&lt;/span> &lt;span style="color: red;">=&lt;/span> &lt;span style="color: blue; font-weight: bold;">do&lt;/span>
&lt;span style="">&gt;&lt;/span>     &lt;span style="">stdgen&lt;/span> &lt;span style="color: red;">&lt;-&lt;/span> &lt;span style="">getStdGen&lt;/span>
&lt;span style="">&gt;&lt;/span>     &lt;span style="">return&lt;/span> &lt;span style="">$&lt;/span> &lt;span style="">g&lt;/span> &lt;span style="">stdgen&lt;/span>
</code></pre>

Use `sequence` to get a few samples. Note that this relies on the `Applicative` instance’s definition of `<*>` to get a new standard number generator each time it is used, which in turn uses the `Monad` instance’s definition which uses `split`.

<pre class="sourceCode haskell"><code class="sourceCode haskell">
&lt;span style="">&gt;&lt;/span> &lt;span style="">samples&lt;/span> &lt;span style="color: red;">::&lt;/span> &lt;span style="">Gen&lt;/span> &lt;span style="">a&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="">IO&lt;/span> &lt;span style="color: red;">[&lt;/span>&lt;span style="">a&lt;/span>&lt;span style="color: red;">]&lt;/span>
&lt;span style="">&gt;&lt;/span> &lt;span style="">samples&lt;/span> &lt;span style="">g&lt;/span> &lt;span style="color: red;">=&lt;/span> &lt;span style="">generate&lt;/span> &lt;span style="">$&lt;/span> &lt;span style="">sequence&lt;/span> &lt;span style="color: red;">[&lt;/span>&lt;span style="">g&lt;/span>&lt;span style="color: red;">,&lt;/span> &lt;span style="">g&lt;/span>&lt;span style="color: red;">,&lt;/span> &lt;span style="">g&lt;/span>&lt;span style="color: red;">,&lt;/span> &lt;span style="">g&lt;/span>&lt;span style="color: red;">,&lt;/span> &lt;span style="">g&lt;/span>&lt;span style="color: red;">,&lt;/span> &lt;span style="">g&lt;/span>&lt;span style="color: red;">,&lt;/span> &lt;span style="">g&lt;/span>&lt;span style="color: red;">,&lt;/span> &lt;span style="">g&lt;/span>&lt;span style="color: red;">,&lt;/span> &lt;span style="">g&lt;/span>&lt;span style="color: red;">]&lt;/span>
</code></pre>

<pre class="sourceCode haskell"><code class="sourceCode haskell">
&lt;span style="">&gt;&lt;/span> &lt;span style="color: blue; font-weight: bold;">instance&lt;/span> &lt;span style="">Arbitrary&lt;/span> &lt;span style="">Int&lt;/span> &lt;span style="color: blue; font-weight: bold;">where&lt;/span>
&lt;span style="">&gt;&lt;/span>   &lt;span style="">arbitrary&lt;/span> &lt;span style="color: red;">=&lt;/span> &lt;span style="">Gen&lt;/span> &lt;span style="">$&lt;/span> &lt;span style="color: red;">\&lt;/span>&lt;span style="">stdgen&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span>
&lt;span style="">&gt;&lt;/span>     &lt;span style="color: blue; font-weight: bold;">let&lt;/span> &lt;span style="color: red;">(&lt;/span>&lt;span style="">r&lt;/span>&lt;span style="color: red;">,&lt;/span> &lt;span style="color: blue; font-weight: bold;">_&lt;/span>&lt;span style="color: red;">)&lt;/span> &lt;span style="color: red;">=&lt;/span> &lt;span style="">randomR&lt;/span> &lt;span style="color: red;">(&lt;/span>&lt;span style="color: green;">-&lt;/span>&lt;span class="hs-num">1000&lt;/span>&lt;span style="color: red;">,&lt;/span> &lt;span class="hs-num">1000&lt;/span>&lt;span style="color: red;">)&lt;/span> &lt;span style="">stdgen&lt;/span> &lt;span style="color: blue; font-weight: bold;">in&lt;/span> &lt;span style="">r&lt;/span>
&lt;span style="">&gt;&lt;/span> 
&lt;span style="">&gt;&lt;/span> &lt;span style="">choose&lt;/span> &lt;span style="color: red;">::&lt;/span> &lt;span style="">Random&lt;/span> &lt;span style="">a&lt;/span> &lt;span style="color: red;">=&gt;&lt;/span> &lt;span style="color: red;">(&lt;/span>&lt;span style="">a&lt;/span>&lt;span style="color: red;">,&lt;/span>&lt;span style="">a&lt;/span>&lt;span style="color: red;">)&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="">Gen&lt;/span> &lt;span style="">a&lt;/span>
&lt;span style="">&gt;&lt;/span> &lt;span style="">choose&lt;/span> &lt;span style="">range&lt;/span> &lt;span style="color: red;">=&lt;/span> &lt;span style="">Gen&lt;/span> &lt;span style="">$&lt;/span> &lt;span style="color: red;">\&lt;/span>&lt;span style="">stdgen&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span>
&lt;span style="">&gt;&lt;/span>   &lt;span style="color: blue; font-weight: bold;">let&lt;/span> &lt;span style="color: red;">(&lt;/span>&lt;span style="">r&lt;/span>&lt;span style="color: red;">,&lt;/span> &lt;span style="color: blue; font-weight: bold;">_&lt;/span>&lt;span style="color: red;">)&lt;/span> &lt;span style="color: red;">=&lt;/span> &lt;span style="">randomR&lt;/span> &lt;span style="">range&lt;/span> &lt;span style="">stdgen&lt;/span> &lt;span style="color: blue; font-weight: bold;">in&lt;/span> &lt;span style="">r&lt;/span>
&lt;span style="">&gt;&lt;/span> 
&lt;span style="">&gt;&lt;/span> &lt;span style="">vectorOf&lt;/span> &lt;span style="color: red;">::&lt;/span> &lt;span style="">Int&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="">Gen&lt;/span> &lt;span style="">a&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="">Gen&lt;/span> &lt;span style="color: red;">[&lt;/span>&lt;span style="">a&lt;/span>&lt;span style="color: red;">]&lt;/span>
&lt;span style="">&gt;&lt;/span> &lt;span style="">vectorOf&lt;/span> &lt;span style="color: red;">=&lt;/span> &lt;span style="">replicateM&lt;/span>
&lt;span style="">&gt;&lt;/span> 
&lt;span style="">&gt;&lt;/span> &lt;span style="">listOf&lt;/span> &lt;span style="color: red;">::&lt;/span> &lt;span style="">Gen&lt;/span> &lt;span style="">a&lt;/span> &lt;span style="color: red;">-&gt;&lt;/span> &lt;span style="">Gen&lt;/span> &lt;span style="color: red;">[&lt;/span>&lt;span style="">a&lt;/span>&lt;span style="color: red;">]&lt;/span>
&lt;span style="">&gt;&lt;/span> &lt;span style="">listOf&lt;/span> &lt;span style="">g&lt;/span> &lt;span style="color: red;">=&lt;/span> &lt;span style="color: blue; font-weight: bold;">do&lt;/span>
&lt;span style="">&gt;&lt;/span>   &lt;span style="">k&lt;/span> &lt;span style="color: red;">&lt;-&lt;/span> &lt;span style="">choose&lt;/span> &lt;span style="color: red;">(&lt;/span>&lt;span class="hs-num">1&lt;/span>&lt;span style="color: red;">,&lt;/span> &lt;span class="hs-num">10&lt;/span>&lt;span style="color: red;">)&lt;/span>
&lt;span style="">&gt;&lt;/span>   &lt;span style="">vectorOf&lt;/span> &lt;span style="">k&lt;/span> &lt;span style="">g&lt;/span>
&lt;span style="">&gt;&lt;/span> 
&lt;span style="">&gt;&lt;/span> &lt;span style="color: blue; font-weight: bold;">instance&lt;/span> &lt;span style="">Arbitrary&lt;/span> &lt;span style="">a&lt;/span> &lt;span style="color: red;">=&gt;&lt;/span> &lt;span style="">Arbitrary&lt;/span> &lt;span style="color: red;">[&lt;/span>&lt;span style="">a&lt;/span>&lt;span style="color: red;">]&lt;/span> &lt;span style="color: blue; font-weight: bold;">where&lt;/span>
&lt;span style="">&gt;&lt;/span>   &lt;span style="">arbitrary&lt;/span> &lt;span style="color: red;">=&lt;/span> &lt;span style="">listOf&lt;/span> &lt;span style="">arbitrary&lt;/span>
</code></pre>