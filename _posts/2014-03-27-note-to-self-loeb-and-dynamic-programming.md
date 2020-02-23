---
id: 828
title: 'Note to self: loeb and dynamic programming'
date: 2014-03-27T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2014/03/27/note-to-self-loeb-and-dynamic-programming/
permalink: /2014/03/27/note-to-self-loeb-and-dynamic-programming/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
Note to self: solving a dynamic programming problem using [löb](http://carlo-hamalainen.net/blog/2013/11/28/note-to-self-loeb-and-moeb). Literate Haskell source for this post is here: [DPloeb.lhs](https://github.com/carlohamalainen/playground/blob/master/haskell/dp/DPloeb.lhs).

<pre>&gt; module DPloeb where
</pre>

The first solution is taken from <http://blog.mno2.org/blog/2011/11/28/writing-dynamic-programming-in-haskell>.

<pre>&gt; coins :: [Int]
&gt; coins = [1, 2, 5, 10, 20, 50, 100, 200]
&gt;
&gt; sol1 :: Int -&gt; Int
&gt; sol1 = (!!) (ways2 coins)
&gt;     where ways1 [] = 1 : repeat 0
&gt;           ways1 (c:cs) = n
&gt;               where n = zipWith (+) (ways1 cs) (replicate c 0 ++ n)
&gt;
&gt; main1 :: IO ()
&gt; main1 = print $ sol1 200
</pre>

There are 73682 ways to select coins of the denominations specified in coins that sum to 200: 

<pre>&gt; *DPloeb&gt; main1
&gt; 73682
</pre>

First step: roll out the parameter to way: 

<pre>&gt; ways2 :: [Int] -&gt; [Int]
&gt; ways2 [] = 1 : repeat 0
&gt;
&gt; ways2 [1, 2, 5, 10, 20, 50, 100, 200] = n
&gt;     where c  = 1
&gt;           cs = [2, 5, 10, 20, 50, 100, 200]
&gt;           n  = zipWith (+) (ways2 cs) (replicate c 0 ++ n)
&gt;
&gt; ways2 [2, 5, 10, 20, 50, 100, 200] = n
&gt;     where c  = 2
&gt;           cs = [5, 10, 20, 50, 100, 200]
&gt;           n  = zipWith (+) (ways2 cs) (replicate c 0 ++ n)
&gt;
&gt; ways2 [5, 10, 20, 50, 100, 200] = n
&gt;     where c  = 5
&gt;           cs = [10, 20, 50, 100, 200]
&gt;           n  = zipWith (+) (ways2 cs) (replicate c 0 ++ n)
&gt;
&gt; ways2 [10, 20, 50, 100, 200] = n
&gt;     where c  = 10
&gt;           cs = [20, 50, 100, 200]
&gt;           n  = zipWith (+) (ways2 cs) (replicate c 0 ++ n)
&gt;
&gt; ways2 [20, 50, 100, 200] = n
&gt;     where c  = 20
&gt;           cs = [50, 100, 200]
&gt;           n  = zipWith (+) (ways2 cs) (replicate c 0 ++ n)
&gt;
&gt; ways2 [50, 100, 200] = n
&gt;     where c  = 50
&gt;           cs = [100, 200]
&gt;           n  = zipWith (+) (ways2 cs) (replicate c 0 ++ n)
&gt;
&gt; ways2 [100, 200] = n
&gt;     where c  = 100
&gt;           cs = [200]
&gt;           n  = zipWith (+) (ways2 cs) (replicate c 0 ++ n)
&gt;
&gt; ways2 [200] = n
&gt;     where c  = 200
&gt;           cs = []
&gt;           n  = zipWith (+) (ways2 cs) (replicate c 0 ++ n)
&gt;
&gt; sol2 :: Int -&gt; Int
&gt; sol2 = (!!) (ways2 [1,2,5,10,20,50,100,200])
&gt;
&gt; main2 :: IO ()
&gt; main2 = print $ sol2 200
</pre>

Next, change the parameter from [Int] to Int by indexing on integers instead of various lists: 

<pre>&gt; ways3 :: Int -&gt; [Int]
&gt;
&gt; ways3 0 = 1 : repeat 0
&gt;
&gt; ways3 1 = n
&gt;     where c  = 1
&gt;           cs = 2
&gt;           n  = zipWith (+) (ways3 cs) (replicate c 0 ++ n)
&gt;
&gt; ways3 2 = n
&gt;     where c  = 2
&gt;           cs = 3
&gt;           n  = zipWith (+) (ways3 cs) (replicate c 0 ++ n)
&gt;
&gt; ways3 3 = n
&gt;     where c  = 5
&gt;           cs = 4
&gt;           n  = zipWith (+) (ways3 cs) (replicate c 0 ++ n)
&gt;
&gt; ways3 4 = n
&gt;     where c  = 10
&gt;           cs = 5
&gt;           n  = zipWith (+) (ways3 cs) (replicate c 0 ++ n)
&gt;
&gt; ways3 5 = n
&gt;     where c  = 20
&gt;           cs = 6
&gt;           n  = zipWith (+) (ways3 cs) (replicate c 0 ++ n)
&gt;
&gt; ways3 6 = n
&gt;     where c  = 50
&gt;           cs = 7
&gt;           n  = zipWith (+) (ways3 cs) (replicate c 0 ++ n)
&gt;
&gt; ways3 7 = n
&gt;     where c  = 100
&gt;           cs = 8
&gt;           n  = zipWith (+) (ways3 cs) (replicate c 0 ++ n)
&gt;
&gt; ways3 8 = n
&gt;     where c  = 200
&gt;           cs = 0
&gt;           n  = zipWith (+) (ways3 cs) (replicate c 0 ++ n)
&gt;
&gt; sol3 :: Int -&gt; Int
&gt; sol3 x = ways3 1 !! x
&gt;
&gt; main3 :: IO ()
&gt; main3 = print $ sol3 200
</pre>

Substitute the value of cs in the body of the where clauses: 

<pre>&gt; sol4 :: [Int]
&gt; sol4 = ways1
&gt;   where
&gt;
&gt;   ways0 = 1 : repeat 0
&gt;
&gt;   ways1 = n
&gt;       where c  = 1
&gt;             n  = zipWith (+) ways2 (replicate c 0 ++ n)
&gt;
&gt;   ways2 = n
&gt;       where c  = 2
&gt;             n  = zipWith (+) ways3 (replicate c 0 ++ n)
&gt;
&gt;   ways3 = n
&gt;       where c  = 5
&gt;             n  = zipWith (+) ways4 (replicate c 0 ++ n)
&gt;
&gt;   ways4 = n
&gt;       where c  = 10
&gt;             n  = zipWith (+) ways5 (replicate c 0 ++ n)
&gt;
&gt;   ways5 = n
&gt;       where c  = 20
&gt;             n  = zipWith (+) ways6 (replicate c 0 ++ n)
&gt;
&gt;   ways6 = n
&gt;       where c  = 50
&gt;             n  = zipWith (+) ways7 (replicate c 0 ++ n)
&gt;
&gt;   ways7 = n
&gt;       where c  = 100
&gt;             n  = zipWith (+) ways8 (replicate c 0 ++ n)
&gt;
&gt;   ways8 = n
&gt;       where c  = 200
&gt;             n  = zipWith (+) ways0 (replicate c 0 ++ n)
&gt;
&gt; main4 :: IO ()
&gt; main4 = print $ sol4 !! 200
</pre>

Substitute the value of c in the body of the where clauses: 

<pre>&gt; sol5 :: [Int]
&gt; sol5 = ways1
&gt;   where
&gt;   ways0 = 1 : repeat 0
&gt;
&gt;   ways1 = zipWith (+) ways2 (replicate 1   0 ++ ways1)
&gt;   ways2 = zipWith (+) ways3 (replicate 2   0 ++ ways2)
&gt;   ways3 = zipWith (+) ways4 (replicate 5   0 ++ ways3)
&gt;   ways4 = zipWith (+) ways5 (replicate 10  0 ++ ways4)
&gt;   ways5 = zipWith (+) ways6 (replicate 20  0 ++ ways5)
&gt;   ways6 = zipWith (+) ways7 (replicate 50  0 ++ ways6)
&gt;   ways7 = zipWith (+) ways8 (replicate 100 0 ++ ways7)
&gt;   ways8 = zipWith (+) ways0 (replicate 200 0 ++ ways8)
&gt;
&gt; main5 :: IO ()
&gt; main5 = print $ sol5 !! 200
</pre>

Now tweak the type and collect each of the clauses into a list: 

<pre>&gt; ways6 :: [[Int]]
&gt; ways6 = let ways0 = 1 : repeat 0
&gt;             ways1 = zipWith (+) ways2 (replicate 1   0 ++ ways1)
&gt;             ways2 = zipWith (+) ways3 (replicate 2   0 ++ ways2)
&gt;             ways3 = zipWith (+) ways4 (replicate 5   0 ++ ways3)
&gt;             ways4 = zipWith (+) ways5 (replicate 10  0 ++ ways4)
&gt;             ways5 = zipWith (+) ways6 (replicate 20  0 ++ ways5)
&gt;             ways6 = zipWith (+) ways7 (replicate 50  0 ++ ways6)
&gt;             ways7 = zipWith (+) ways8 (replicate 100 0 ++ ways7)
&gt;             ways8 = zipWith (+) ways0 (replicate 200 0 ++ ways8)
&gt;             ways = [ways0, ways1, ways2, ways3, ways4, ways5, ways6, ways7, ways8]
&gt;             in ways
&gt;
&gt; sol6 :: Int -&gt; Int
&gt; sol6 x = ways6 !! 1 !! x
&gt;
&gt; main6 :: IO ()
&gt; main6 = print $ sol6 200
</pre>

Instead of referring to whys4, index into the list with why !! 4: 

<pre>&gt; ways7 :: [[Int]]
&gt; ways7 = let ways0 = 1 : repeat 0
&gt;             ways1 = zipWith (+) (ways !! 2) (replicate 1   0 ++ (ways !! 1))
&gt;             ways2 = zipWith (+) (ways !! 3) (replicate 2   0 ++ (ways !! 2))
&gt;             ways3 = zipWith (+) (ways !! 4) (replicate 5   0 ++ (ways !! 3))
&gt;             ways4 = zipWith (+) (ways !! 5) (replicate 10  0 ++ (ways !! 4))
&gt;             ways5 = zipWith (+) (ways !! 6) (replicate 20  0 ++ (ways !! 5))
&gt;             ways6 = zipWith (+) (ways !! 7) (replicate 50  0 ++ (ways !! 6))
&gt;             ways7 = zipWith (+) (ways !! 8) (replicate 100 0 ++ (ways !! 7))
&gt;             ways8 = zipWith (+) (ways !! 0) (replicate 200 0 ++ (ways !! 8))
&gt;             ways = [ways0, ways1, ways2, ways3, ways4, ways5, ways6, ways7, ways8]
&gt;             in ways
&gt;
&gt; sol7 :: Int -&gt; Int
&gt; sol7 x = ways7 !! 1 !! x
&gt;
&gt; main7 :: IO ()
&gt; main7 = print $ sol7 200
</pre>

Now we can define ways directly as a list: 

<pre>&gt; ways8 :: [[Int]]
&gt; ways8 = let ways = [ 1 : repeat 0
&gt;                    , zipWith (+) (ways !! 2) (replicate 1   0 ++ (ways !! 1))
&gt;                    , zipWith (+) (ways !! 3) (replicate 2   0 ++ (ways !! 2))
&gt;                    , zipWith (+) (ways !! 4) (replicate 5   0 ++ (ways !! 3))
&gt;                    , zipWith (+) (ways !! 5) (replicate 10  0 ++ (ways !! 4))
&gt;                    , zipWith (+) (ways !! 6) (replicate 20  0 ++ (ways !! 5))
&gt;                    , zipWith (+) (ways !! 7) (replicate 50  0 ++ (ways !! 6))
&gt;                    , zipWith (+) (ways !! 8) (replicate 100 0 ++ (ways !! 7))
&gt;                    , zipWith (+) (ways !! 0) (replicate 200 0 ++ (ways !! 8))
&gt;                    ]
&gt;             in ways
&gt;
&gt; sol8 :: Int -&gt; Int
&gt; sol8 x = ways8 !! 1 !! x
&gt;
&gt; main8 :: IO ()
&gt; main8 = print $ sol8 200
</pre>

Factor out whys by writing each list element as a function. 

<pre>&gt; ways9 :: [[Int]]
&gt; ways9 = let fs   = [ const $ 1 : repeat 0
&gt;                    , w -&gt; zipWith (+) (w !! 2) (replicate 1   0 ++ (w !! 1))
&gt;                    , w -&gt; zipWith (+) (w !! 3) (replicate 2   0 ++ (w !! 2))
&gt;                    , w -&gt; zipWith (+) (w !! 4) (replicate 5   0 ++ (w !! 3))
&gt;                    , w -&gt; zipWith (+) (w !! 5) (replicate 10  0 ++ (w !! 4))
&gt;                    , w -&gt; zipWith (+) (w !! 6) (replicate 20  0 ++ (w !! 5))
&gt;                    , w -&gt; zipWith (+) (w !! 7) (replicate 50  0 ++ (w !! 6))
&gt;                    , w -&gt; zipWith (+) (w !! 8) (replicate 100 0 ++ (w !! 7))
&gt;                    , w -&gt; zipWith (+) (w !! 0) (replicate 200 0 ++ (w !! 8))
&gt;                    ]
&gt;             ways = map ($ ways) fs
&gt;             in ways
&gt;
&gt; sol9 :: Int -&gt; Int
&gt; sol9 x = ways9 !! 1 !! x
&gt;
&gt; main9 :: IO ()
&gt; main9 = print $ sol9 200
</pre>

Now use loeb: 

<pre>&gt; loeb :: Functor f =&gt; f (f b -&gt; b) -&gt; f b
&gt; loeb fs = go where go = fmap ($ go) fs
&gt;
&gt; fs10 :: [[[Int]] -&gt; [Int]]
&gt; fs10   = [ const $ 1 : repeat 0
&gt;          , w -&gt; zipWith (+) (w !! 2) (replicate 1   0 ++ (w !! 1))
&gt;          , w -&gt; zipWith (+) (w !! 3) (replicate 2   0 ++ (w !! 2))
&gt;          , w -&gt; zipWith (+) (w !! 4) (replicate 5   0 ++ (w !! 3))
&gt;          , w -&gt; zipWith (+) (w !! 5) (replicate 10  0 ++ (w !! 4))
&gt;          , w -&gt; zipWith (+) (w !! 6) (replicate 20  0 ++ (w !! 5))
&gt;          , w -&gt; zipWith (+) (w !! 7) (replicate 50  0 ++ (w !! 6))
&gt;          , w -&gt; zipWith (+) (w !! 8) (replicate 100 0 ++ (w !! 7))
&gt;          , w -&gt; zipWith (+) (w !! 0) (replicate 200 0 ++ (w !! 8))
&gt;          ]
&gt;
&gt; sol10 :: Int -&gt; Int
&gt; sol10 x = loeb fs10 !! 1 !! x
&gt;
&gt; main10 :: IO ()
&gt; main10 = print $ sol10 200
</pre>

Finally, we can generalise this solution by writing a function to produce the elements of fs10: 

<pre>&gt; make :: [Int] -&gt; Int -&gt; ([[Int]] -&gt; [Int])
&gt; make _  0 = const $ 1 : repeat 0
&gt; make cs k = if k == length cs
&gt;                 then w -&gt; zipWith (+) (w !! 0)     (replicate (cs !! (k-1)) 0 ++ (w !! k))
&gt;                 else w -&gt; zipWith (+) (w !! (k+1)) (replicate (cs !! (k-1)) 0 ++ (w !! k))
&gt;
&gt; sol11 :: Int -&gt; Int
&gt; sol11 x = result !! 1 !! x
&gt;   where result = loeb $ map (make coins) [0..length coins]
&gt;
&gt; main11 :: IO ()
&gt; main11 = print $ sol11 200
</pre>

These all say 73682: 

<pre>&gt; mains :: IO ()
&gt; mains = do main1
&gt;            main2
&gt;            main3
&gt;            main4
&gt;            main5
&gt;            main6
&gt;            main7
&gt;            main8
&gt;            main9
&gt;            main10
&gt;            main11
</pre>