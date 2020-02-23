---
id: 733
title: Circus problem
date: 2015-01-04T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2015/01/04/circus-problem/
permalink: /2015/01/04/circus-problem/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
A coding-challenge kind of question, and a short foray into QuickCheck. Source is here: <https://github.com/carlohamalainen/playground/tree/master/haskell/circus>.

Here is a question from [Cracking the Coding Interview, 4th Edition](http://www.amazon.com/Cracking-Coding-Interview-Programming-Questions/dp/098478280X): 

<pre>9.7 A circus is designing a tower routine consisting of people
standing atop one another's shoulders. For practical and aesthetic
reasons, each person must be both shorter and lighter than the person
below him or her. Given the heights and weights of each person in
the circus, write a method to compute the largest possible number of
people in such a tower.

EXAMPLE:

Input (ht, wt): (65, 100) (70, 150) (56, 90) (75, 190) (60, 95)
(68, 110)

Output: The longest tower is length 6 and includes from top to bottom:
(56, 90) (60,95) (65,100) (68,110) (70,150) (75,190)
</pre>

The provided [solution](https://github.com/carlohamalainen/playground/blob/master/haskell/circus/JavaSoln.java) is written in Java and seems far too verbose for such a simple problem. 

For contrast, here’s one way to solve it in Haskell: 

<pre>&gt; {-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
&gt;
&gt; module Circus where
&gt;
&gt; import Control.Applicative
&gt; import Data.Function (on)
&gt; import Data.List (inits, maximumBy, sort)
&gt; import Test.QuickCheck
&gt; import Test.QuickCheck.Modifiers()
</pre>

Model a person by an algebraic data type. The compiler is smart enough to work out the instances for Eq, Ord, and Show for us. The Ord instance works as if we had defined the person as a tuple of ints. 

<pre>&gt; data Person = Person Int Int deriving (Eq, Ord, Show)
</pre>

A valid list of people must satisfy the property that each person is strictly less than the person next in the list. We can check this by pattern matching on the list. If there are two people p1, p2, then the list is valid if p1 < p2 and the remaining list is also valid. Other cases (the empty list or a list with a single person) are valid by definition.

<pre>&gt; validSeries :: [Person] -&gt; Bool
&gt; validSeries (p1:p2:px) = p1  validSeries _          = True
</pre>

Checking this function: 

<pre>*Circus&gt; validSeries [Person 100 65, Person 150 80]
True

*Circus&gt; validSeries [Person 100 65, Person 150 80, Person 140 70]
False
</pre>

Given a **sorted** list of people, we are interested in the longest valid sequence from the start of the list. The inits function provides all initial lists:

<pre>*Circus&gt; inits [Person 100 65, Person 150 80, Person 140 70]
[
  [],
  [Person 100 65],
  [Person 100 65, Person 150 80],
  [Person 100 65, Person 150 80, Person 140 70]
]
</pre>

So we want to keep taking elements from inits of our list of people, while the list is a valid series. The takeWhile function does, taking a boolean predicate as its first argument. Finally, we want the longest such list, so we take the last element which is the longest one. 

<pre>&gt; longestInit :: [Person] -&gt; [Person]
&gt; longestInit ps = last $ takeWhile validSeries $ inits ps
</pre>

To me this is easier to read than the rough equivalent in Java, which involves some gnarly loops and a return value that encodes detail by returning the value of startFrom when the loop doesn’t have enough items to consume: 

<pre>int fillNextSeq(int startFrom, ArrayList seq) {
    int firstUnfitItem = startFrom;
    if (startFrom &lt; items.size()) {
        for (int i = 0; i &lt; items.size(); i++) {
            HtWt item = items.get(i);
            if (i == 0 || items.get(i-1).isBefore(item)) {
                seq.add(item);
            } else {
                firstUnfitItem = i;
            }
        }
    }
    return firstUnfitItem;
}
</pre>

Not my cup of tea! 

Finally, to find the solution we find the longest sequence by sorting the input and looking for the longest sequence from the initial sequence and those in any later lists: 

<pre>&gt; soln0 :: [Person] -&gt; [Person]
&gt; soln0 px = maximumBy (compare `on` length) (findLongestSeq (sort px))
&gt;
&gt; findLongestSeq :: [Person] -&gt; [[Person]]
&gt; findLongestSeq [] = []
&gt; findLongestSeq ps = initSeq:(findLongestSeq rest)
&gt;   where
&gt;     initSeq = longestInit ps
&gt;     rest    = drop (length initSeq) ps
</pre>

Let’s try it out on the question’s sample data: 

<pre>&gt; sampleInput :: [Person]
&gt; sampleInput = [ Person 65 100
&gt;               , Person 70 150
&gt;               , Person 56 90
&gt;               , Person 75 190
&gt;               , Person 60 95
&gt;               , Person 68 110
&gt;               ]
</pre>

<pre>*Circus&gt; soln0 sampleInput
[Person 56 90,Person 60 95,Person 65 100,Person 68 110,Person 70 150,Person 75 190]

*Circus&gt; length $ soln0 sampleInput
6
</pre>

But soln0 has a bug &#8211; an unhandled case. Can you spot it? 

Let’s write an instance for Arbitrary so that we can use [QuickCheck](https://www.haskell.org/haskellwiki/Introduction_to_QuickCheck2). 

<pre>&gt; instance Arbitrary Person where
&gt;     arbitrary = Person  pos1  pos2
&gt;         where pos1 = getPositive  arbitrary
&gt;               pos2 = getPositive  arbitrary
</pre>

We have used getPositive from [Test.QuickCheck.Modifiers](https://hackage.haskell.org/package/QuickCheck-2.7.3/docs/Test-QuickCheck-Modifiers.html) because a person cannot have negative height or weight.

One property that we can check is that the solution does not change even if we reverse the input list. (We’d expect this to hold because we sort the list before looking for longest strictly increasing sequences.) Here is a function to check this property:

<pre>&gt; qc_soln0_1 = quickCheck (pl -&gt; soln0 pl == soln0 (reverse pl))
</pre>

It blows up, and QuickCheck provides us with the smallest input that causes a problem: 

<pre>*Circus&gt; qc_soln0_1
*** Failed! Exception: 'List.maximumBy: empty list' (after 1 test):
[]
</pre>

In this case, it’s the empty list being supplied to maximumBy. Indeed, findLongestSeq on an empty list is just the empty list: 

<pre>*Circus&gt; findLongestSeq []
[]

*Circus&gt; maximumBy (compare `on` length) []
*** Exception: List.maximumBy: empty list
</pre>

Perhaps we should have used a [Safe](https://ghc.haskell.org/trac/ghc/wiki/SafeHaskell) variant of maximumBy. In any case, we have to handle the situation where the result is an empty list: 

<pre>&gt; soln :: [Person] -&gt; [Person]
&gt; soln px = if result == [] then [] else maximumBy (compare `on` length) result
&gt;   where
&gt;     result = findLongestSeq $ sort px
</pre>

This definition appears to be correct, and passes all of the following tests: 

<pre>&gt; -- No change if we reverse the input list:
&gt; qc_soln_1 = quickCheck (pl -&gt; soln pl == soln (reverse pl))
&gt;
&gt; -- Computing the solution of the solution is identical to the solution:
&gt; qc_soln_2 = quickCheck (pl -&gt; soln pl == (soln . soln) pl)
&gt;
&gt; -- Computing the solution of the solution of the solution is idential to the original solution:
&gt; qc_soln_3 = quickCheck (pl -&gt; soln pl == (soln . soln . soln) pl)
&gt;
&gt; -- The length of the solution is not longer than the input:
&gt; qc_soln_4 = quickCheck (pl -&gt; length (soln pl) 
&gt; -- A solution can be extended in a valid way, and solving on that input gives an
&gt; -- answer of length (original solution) + 1:
&gt; qc_soln_5 = quickCheck (pl -&gt; let pl' = extendByOne (soln pl) in length pl' == length (soln pl) + 1)
&gt;   where
&gt;     extendByOne :: [Person] -&gt; [Person]
&gt;     extendByOne [] = [Person 1 1]
&gt;     extendByOne ps = ps ++ [Person (h + 1) (w + 1)]
&gt;       where
&gt;         Person h w = last ps
&gt;
&gt; -- Extending the input with a duplicate value gives a solution that is never longer
&gt; -- than the original solution:
&gt; qc_soln_6 = quickCheck (pl -&gt; length (soln pl) &gt;= length (soln $ extendWithDup pl))
&gt;   where
&gt;     extendWithDup :: [Person] -&gt; [Person]
&gt;     extendWithDup [] = []
&gt;     extendWithDup ps = ps ++ [last ps]
</pre>

Verifying these properties at the repl: 

<pre>*Circus&gt; qc_soln_1
+++ OK, passed 100 tests.

*Circus&gt; qc_soln_2
+++ OK, passed 100 tests.

*Circus&gt; qc_soln_3
+++ OK, passed 100 tests.

*Circus&gt; qc_soln_4
+++ OK, passed 100 tests.

*Circus&gt; qc_soln_5
+++ OK, passed 100 tests.

*Circus&gt; qc_soln_6
+++ OK, passed 100 tests.
</pre>

This is a fairly basic use of QuickCheck. For more detail about its background and some examples from the Erlang version of the library, check out this talk by John Hughes: 

<div class="jetpack-video-wrapper">
  <span class="embed-youtube" style="text-align:center; display: block;"></span>
</div>