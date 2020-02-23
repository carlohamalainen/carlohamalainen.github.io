---
id: 761
title: 'Note to self: exceptions in multithreaded Haskell'
date: 2015-05-25T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2015/05/25/note-to-self-exceptions-in-multithreaded-haskell/
permalink: /2015/05/25/note-to-self-exceptions-in-multithreaded-haskell/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
Note to self on catching exceptions in multithreaded Haskell code. Literate Haskell source and build scripts and cabal stuff is at <https://github.com/carlohamalainen/playground/tree/master/haskell/exceptions-in-parallel>. 

For my use cases there are two scenarios when running a list of worker threads: 

<ol style="list-style-type:decimal;">
  <li>
    If any thread throws an exception, give up on everything.
  </li>
  <li>
    If any thread throws an exception, log it, but let the other workers run to completion.
  </li>
</ol>

First, imports that we’ll use: 

<pre>&gt; {-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
&gt;
&gt; module Main where
&gt;
&gt; import Data.Conduit
&gt; import Data.Conduit.List
&gt; import Data.Traversable (traverse)
&gt; import Control.Applicative
&gt; import Control.Monad.Catch
&gt; import Control.Concurrent
&gt; import Control.Concurrent.Async
&gt; import Control.Concurrent.ParallelIO.Local
&gt; import Control.Monad hiding (mapM, mapM_)
&gt; import Control.Monad.Catch
&gt; import Data.Typeable
&gt; import Prelude hiding (map, mapM, mapM_)
&gt; import System.IO
</pre>

We will use code from [parallel-io](https://hackage.haskell.org/package/parallel-io) and [async](https://hackage.haskell.org/package/async) for running worker threads. For a pipeline we’ll also use [conduit](http://hackage.haskell.org/package/conduit-1.2.4.2/docs/Data-Conduit.html). 

Here’s our exception type, which we throw using throwM from [Control.Monad.Catch](https://hackage.haskell.org/package/exceptions-0.8.0.2/docs/Control-Monad-Catch.html): 

<pre>&gt; data MyException = MyException String deriving (Show, Typeable)
&gt;
&gt; instance Exception MyException
</pre>

Our two tasks. The first task immediately throws an exception; the second waits for 5 seconds and completes happily. 

<pre>&gt; task1 :: IO String
&gt; task1 = throwM $ MyException "task1 blew up"
&gt;
&gt; task2 :: IO String
&gt; task2 = do
&gt;   threadDelay $ 5 * 10^6
&gt;   return $ "task2 finished"
</pre>

**Example: parallel_** 

<pre>&gt; main1 :: IO ()
&gt; main1 = do
&gt;
&gt;   x  parallel_ pool [task1, task2]
&gt;   print (x :: ())
</pre>

Output: 

<pre>*Main&gt; main1
*** Exception: MyException "task1 blew up"
</pre>

**Example: parallelE_** 

<pre>&gt; main2 :: IO ()
&gt; main2 = do
&gt;
&gt;   x  parallelE_ pool [task1, task2]
&gt;   print x
</pre>

Output: 

<pre>*Main&gt; main2
[Just (MyException "task1 blew up"),Nothing]
</pre>

**Example: parallel** 

<pre>&gt; main3 :: IO ()
&gt; main3 = do
&gt;   x  parallel pool [task1, task2]
&gt;   print x
</pre>

Output: 

<pre>*Main&gt; main3
*** Exception: MyException "task1 blew up"
</pre>

**Example: parallelE** 

<pre>&gt; main4 :: IO ()
&gt; main4 = do
&gt;   x  parallelE pool [task1, task2]
&gt;   print x
</pre>

Output: 

<pre>*Main&gt; main4
[Left (MyException "task1 blew up"),Right "task2 finished"]
</pre>

**Example: async/wait** 

<pre>&gt; main5 :: IO ()
&gt; main5 = do
&gt;   a1    a2    result1    result2 
&gt;   print [result1, result2]
</pre>

Output: 

<pre>*Main&gt; main5
*** Exception: MyException "task1 blew up"
</pre>

**Example: async/waitCatch** 

<pre>&gt; main6 :: IO ()
&gt; main6 = do
&gt;   a1    a2    result1    result2 
&gt;   print [result1, result2]
</pre>

Output: 

<pre>*Main&gt; main6
[Left (MyException "task1 blew up"),Right "task2 finished"]
</pre>

**Example: concurrently** 

<pre>&gt; main7 :: IO ()
&gt; main7 = do
&gt;   result 
&gt;   print result
</pre>

Output: 

<pre>*Main&gt; main7
*** Exception: MyException "task1 blew up"
</pre>

**Example: throwM in a conduit sink** 

<pre>&gt; main8 :: IO ()
&gt; main8 = do
&gt;   sourceList [1..5] $$ (throwM $ MyException "main8 in conduit exploded")
&gt;   print "this is never printed"
</pre>

Output: 

<pre>*** Exception: MyException "main8 in conduit exploded"
</pre>

**Example: throwM in a conduit sink (on one value)** 

<pre>&gt; main9 :: IO ()
&gt; main9 = do
&gt;
&gt;   let foo x = if x == 3 then throwM $ MyException "got a 3 in main9"
&gt;                         else print x
&gt;
&gt;   sourceList [1..5] $$ (mapM_ foo)
&gt;   print "this is never printed"
</pre>

The conduit processes values 1 and 2, throws an exception on 3, and never sees 4 and 5. 

<pre>*Main&gt; main9
1
2
*** Exception: MyException "got a 3 in main9"
</pre>

**Example: throwM/catchC** 

<pre>&gt; main10 :: IO ()
&gt; main10 = do
&gt;
&gt;   let foo x = if x == 3 then throwM $ MyException "got a 3 in main10"
&gt;                         else print x
&gt;
&gt;   let sink = catchC (mapM_ foo)
&gt;                     ((e :: SomeException) -&gt; mapM_ $ x -&gt; putStrLn $ "When processing " ++ show x ++ " caught exception: " ++ show e)
&gt;
&gt;   sourceList [1..5] $$ sink
&gt;   print "main10 finished"
</pre>

The output is not what I expected. Values 1 and 2 are processed as expected, then the 3 throws an exception, but the effect of catchC is that **the rest of the values (4 and 5) are processed using the second argument to catchC**. In this situation, a conduit can’t be used to process a stream with independently failing components. You have to catch all exceptions before they bubble up to the conduit code. 

<pre>1
2
When processing 4 caught exception: MyException "got a 3 in main10"
When processing 5 caught exception: MyException "got a 3 in main10"
"main10 finished"
</pre>

**Example: catchAll in conduit** 

A combinator that runs an IO action and catches any exception: 

<pre>&gt; catchBlah :: Show a =&gt; (a -&gt; IO ()) -&gt; a -&gt; IO ()
&gt; catchBlah action = x -&gt; catchAll (action x)
&gt;                                   ((e :: SomeException) -&gt; putStrLn $ "On value " ++ show x ++ " caught exception: " ++ show e)
</pre>

Using catchBlah in the sink: 

<pre>&gt; main11 :: IO ()
&gt; main11 = do
&gt;
&gt;   let foo x = if x == 3 then throwM $ MyException "got a 3 in main11"
&gt;                         else print x
&gt;
&gt;   sourceList [1..5] $$ (mapM_ $ catchBlah foo)
&gt;
&gt;   print "main11 finished"
</pre>

Now the conduit processes every value, because the exception is caught and dealt with at a lower level. 

<pre>*Main&gt; main11
1
2
On value 3 caught exception: MyException "got a 3 in main11"
4
5
"main11 finished"
</pre>

**Example: catchBlah’ in conduit** 

Now, suppose we have a few stages in the conduit and the first stage blows up. Use catchAll to catch the exception and return a IO (Maybe b) instead of IO b: 

<pre>&gt; catchBlah' :: Show a =&gt; (a -&gt; IO b) -&gt; a -&gt; IO (Maybe b)
&gt; catchBlah' action = x -&gt; do
&gt;   catchAll (action x &gt;&gt;= (return . Just))
&gt;            ((e :: SomeException) -&gt; do putStrLn $ "On value " ++ show x ++ " caught exception: " ++ show e
&gt;                                         return Nothing)
</pre>

<pre>&gt; main12 :: IO ()
&gt; main12 = do
&gt;
&gt;   let src = [1..5] :: [Int]
&gt;
&gt;   let stage1 x = do when (x == 3) $ throwM $ MyException "Got a 3 in stage1"
&gt;                     putStrLn $ "First print: " ++ show x
&gt;                     return x
&gt;
&gt;   sourceList src $$ (mapM $ catchBlah' stage1) =$= (mapM_ print)
&gt;
&gt;   print "main12 finished"
</pre>

Output: 

<pre>First print: 1
Just 1
First print: 2
Just 2
On value 3 caught exception: MyException "Got a 3 in stage1"
Nothing
First print: 4
Just 4
First print: 5
Just 5
"main12 finished"
</pre>

**Example: catchBlah’ in conduit (tweaked)** 

Same as the previous example but with nicer printing in the sink: 

<pre>&gt; main13 :: IO ()
&gt; main13 = do
&gt;
&gt;   let src = [1..5] :: [Int]
&gt;
&gt;   let stage1 x = do when (x == 3) $ throwM $ MyException "Got a 3 in stage1"
&gt;                     putStrLn $ "First print: " ++ show x
&gt;                     return x
&gt;       stage2 x = case x of
&gt;                       Just x' -&gt; do putStrLn $ "Second print: " ++ show (x' + 1)
&gt;                                     putStrLn ""
&gt;                       Nothing -&gt; do putStrLn $ "Second print got Nothing..."
&gt;                                     putStrLn ""
&gt;
&gt;   sourceList src $$ (mapM $ catchBlah' stage1) =$= (mapM_ stage2)
&gt;
&gt;   print "main13 finished"
</pre>

Output: 

<pre>*Main&gt; main13
First print: 1
Second print: 2

First print: 2
Second print: 3

On value 3 caught exception: MyException "Got a 3 in stage1"
Second print got Nothing...

First print: 4
Second print: 5

First print: 5
Second print: 6

"main13 finished"
</pre>