---
id: 795
title: 'note to self: löb and möb'
date: 2013-11-28T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2013/11/28/note-to-self-lob-and-mob/
permalink: /2013/11/28/note-to-self-lob-and-mob/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
Working through the detail of [Löb and möb: strange loops in Haskell](https://github.com/quchen/articles/blob/master/loeb-moeb.md) and the related discussion on [reddit.com/r/haskell](http://www.reddit.com/r/haskell/comments/1qwjk6/l%C3%B6b_and_m%C3%B6b_strange_loops_in_haskell/), in particular [psygnisfive’s comment](http://www.reddit.com/r/haskell/comments/1qwjk6/l%C3%B6b_and_m%C3%B6b_strange_loops_in_haskell/cdhsefm). There’s nothing much original in this post, I’m just working through the details.

The LHS source for this post is here: [https://github.com/carlohamalainen/playground/tree/master/haskell/loeb\_and\_moeb](https://github.com/carlohamalainen/playground/tree/master/haskell/loeb_and_moeb)

<pre>&gt; module Loeb where
</pre>

In a spreadsheet one has a set of cells, and each cell can be defined in terms of values in the other cells. For example, let’s use four cells x0, x1, x2, and x3 with the following definition:

<pre>&gt; f1 :: [Int]
&gt; f1 = let xs0 = 1
&gt;          xs1 = succ xs0
&gt;          xs2 = succ xs1
&gt;          xs3 = succ xs2
&gt;          xs  = [xs0, xs1, xs2, xs3]
&gt;          in xs
</pre>

The variable xs0 appears in a few places but it can be factored out. Remove xs0 = 1 and substitute the value 1 in the definition for xs. Also, xs0 is the first element of the xs list, so refer to it using xs !! 0: 

<pre>&gt; f2 :: [Int]
&gt; f2 = let xs1 = succ (xs !! 0)
&gt;          xs2 = succ xs1
&gt;          xs3 = succ xs2
&gt;          xs = [1, xs1, xs2, xs3]
&gt;          in xs
</pre>

Now do the same for xs1:

<pre>&gt; f3 :: [Int]
&gt; f3 = let xs2 = succ (xs !! 1)
&gt;          xs3 = succ xs2
&gt;          xs = [1, succ (xs !! 0), xs2, xs3]
&gt;          in xs
</pre>

The pattern should be clear, so now factor out xs2 and xs3: 

<pre>&gt; f4 :: [Int]
&gt; f4 = let xs = [ 1
&gt;               , succ (xs !! 0)
&gt;               , succ (xs !! 1)
&gt;               , succ (xs !! 2)
&gt;               ]
&gt;          in xs
</pre>

The common feature of the last three lines is that they are a function of xs. The first line is the constant 1, and we can make this a function of xs with something like 

<pre>&gt; _ -&gt; 1
</pre>

but the standard prelude provides const 1 for just this purpose. So now we have: 

<pre>&gt; f4_1 :: [Int]
&gt; f4_1 = let xs = [ const 1 $ xs
&gt;                 , succ (xs !! 0)
&gt;                 , succ (xs !! 1)
&gt;                 , succ (xs !! 2)
&gt;                 ]
&gt;          in xs
</pre>

So each line is a function of xs. Can we factor it out, in a sense, so each line looks more like the first? Yes: 

<pre>&gt; f4_2 :: [Int]
&gt; f4_2 = let xs = [ const 1 $ xs
&gt;                 , succ . (h -&gt; h !! 0) $ xs
&gt;                 , succ . (h -&gt; h !! 1) $ xs
&gt;                 , succ . (h -&gt; h !! 2) $ xs
&gt;                 ]
&gt;          in xs
</pre>

The lambda expressions are a bit cumbersome. What we are doing is the succ function after selecting a certain element of a list. Haskell supports [currying](http://www.haskell.org/haskellwiki/Currying), and when one curries an operator, the left vs right arguments are respected:

<pre>*Main&gt; :t (!!)
(!!) :: [a] -&gt; Int -&gt; a

*Main&gt; :t (!! 3)
(!! 3) :: [a] -&gt; a
</pre>

So succ (xs !! 0) can be rewritten as succ . (!! 0) $ xs. Here is the next version: 

<pre>&gt; f5 :: [Int]
&gt; f5 = let xs = [ const 1       $ xs
&gt;               , succ . (!! 0) $ xs
&gt;               , succ . (!! 1) $ xs
&gt;               , succ . (!! 2) $ xs
&gt;               ]
&gt;         in xs
</pre>

We can still ask if there is a way to generalise the definition of f5. Each line is of the form function $ xs so we could define a list of functions 

<pre>&gt; fs = [ const 1
&gt;      , succ . (!! 0)
&gt;      , succ . (!! 1)
&gt;      , succ . (!! 2)
&gt;      ]
</pre>

and then xs = map (-> f xs) fs. In full: 

<pre>&gt; f6_1 :: [Int]
&gt; f6_1 = let fs = [ const 1
&gt;                 , succ . (!! 0)
&gt;                 , succ . (!! 1)
&gt;                 , succ . (!! 2)
&gt;                 ]
&gt;            xs = map (f -&gt; f xs) fs
&gt;            in xs
</pre>

Finally, the lambda expression is a bit clunky and Haskell provides the dollar-sign operator for function application (which is all the lambda expression is actually doing). With currying we get an appropriate type: 

<pre>*Main&gt; :t ($)
($) :: (a -&gt; b) -&gt; a -&gt; b

*Main&gt; :t ($ [1, 2, 3])
($ [1, 2, 3]) :: Num t =&gt; ([t] -&gt; b) -&gt; b
</pre>

so ($ xs) will be a function that takes a function that operates on a list and returns something (as long as xs is a list). This is just what we need: 

<pre>&gt; f6_2 :: [Int]
&gt; f6_2 = let fs = [ const 1
&gt;                 , succ . (!! 0)
&gt;                 , succ . (!! 1)
&gt;                 , succ . (!! 2)
&gt;                 ]
&gt;            xs = map ($ xs) fs
&gt;           in xs
</pre>

and this is the final form in [psygnisfive’s](http://www.reddit.com/r/haskell/comments/1qwjk6/l%C3%B6b_and_m%C3%B6b_strange_loops_in_haskell/cdhsefm) comment. 

(Embarrassingly for myself, I had assumed that the type of (!! xs) would be the result of currying on its _left-hand_ parameter, not the right, which made the map ($ xs) fs form incomprehensible.) 

To finish things off, we’d like to write a function that computes the result f6_2 given the list fs. Here’s a first attempt: 

<pre>&gt; loeb1 fs = let xs = map ($ xs) fs in xs
</pre>

An alternative to using a let definition is to use a where (this brings us closer to the form given by [quchen](https://github.com/quchen)): 

<pre>&gt; loeb2 fs = go where go = map ($ go) fs
</pre>

Looking at the type of loeb2, 

<pre>&gt; loeb2 :: [[b] -&gt; b] -&gt; [b]
</pre>

shows the fact that we used a list as the starting point for the derivation. The first parameter is a list of functions that take a list and produce a value (of the same type), and the result is list. The final remark in psygnisfive’s comment is “rinse and repeat for your favourite functor.” What this refers to is the fact that map is specialised for lists. Functors generalise the idea of being able to “map over” something, and fmap generalises map: 

<pre>*Main&gt; :t map
map :: (a -&gt; b) -&gt; [a] -&gt; [b]

*Main&gt; :t fmap
fmap :: Functor f =&gt; (a -&gt; b) -&gt; f a -&gt; f b

*Main&gt; map (+2) [1, 2, 3]
[3,4,5]

*Main&gt; fmap (+2) [1, 2, 3]
[3,4,5]

*Main&gt; :m Data.Maybe

Prelude Data.Maybe&gt; fmap (+1) (Just 3)
Just 4

Prelude Data.Maybe&gt; fmap (+1) Nothing
Nothing
</pre>

Changing map to fmap in the definition of loeb2 gets us the actual definition of loeb: 

<pre>&gt; loeb :: Functor f =&gt; f (f b -&gt; b) -&gt; f b
&gt; loeb fs = go where go = fmap ($ go) fs
</pre>

For what it’s worth, putting f = [] specialises to the type signature of the earlier loeb2: 

<pre>&gt; loeb :: [] ([] b -&gt; b) -&gt; [] b
</pre>

which can be rewritten in the usual form 

<pre>&gt; loeb :: [[b] -&gt; b] -&gt; [b]
</pre>

It doesn’t end! You can then abstract out the fmap by making it a parameter, which gives the moeb function: 

<pre>&gt; moeb :: t -&gt; (((a -&gt; b) -&gt; b) -&gt; t -&gt; a) -&gt; a
&gt; moeb fs x = go where go = x ($ go) fs
</pre>

See the [discussion on reddit](http://www.reddit.com/r/haskell/comments/1qwjk6/l%C3%B6b_and_m%C3%B6b_strange_loops_in_haskell/) for motivation and possible uses of moeb.