---
id: 813
title: Fiddling with GADTs
date: 2013-05-16T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2013/05/16/fiddling-with-gadts/
permalink: /2013/05/16/fiddling-with-gadts/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
<https://gist.github.com/carlohamalainen/5589736.js>

Check the depth of the structure: 

<pre>*Main&gt; depth T0
0
*Main&gt; depth (Tm T0)
1
*Main&gt; depth (Tm (Tm (Tm (Tm T0))))
4
</pre>

The function twoOfSameDepth expects both parameters to be of the same depth. If not, you get a type error. 

<pre>*Main&gt; :t twoOfSameDepth T0 T0
twoOfSameDepth T0 T0 :: Int
*Main&gt; :t twoOfSameDepth T0 (Tm T0)

:1:20:
    Couldn't match expected type `'Z' with actual type `S n0'
    Expected type: T 'Z
      Actual type: T (S n0)
    In the return type of a call of `Tm'
    In the second argument of `twoOfSameDepth', namely `(Tm T0)'
</pre>

The function secondIsPlusOne expects the second parameter to be of depth one more than the first parameter: 

<pre>*Main&gt; :t secondIsPlusOne T0 (Tm T0)
secondIsPlusOne T0 (Tm T0) :: Bool
*Main&gt; :t secondIsPlusOne T0 (Tm (Tm T0))

:1:25:
    Couldn't match expected type `'Z' with actual type `S n0'
    Expected type: T 'Z
      Actual type: T (S n0)
    In the return type of a call of `Tm'
    In the first argument of `Tm', namely `(Tm T0)'
*Main&gt;
</pre>

In [Matt&#8217;s LambdaJam talk/jam](http://matthew.brecknell.net/post/btree-gadt/) you can see how this idea can be used to help with implementing a b-tree data structure.