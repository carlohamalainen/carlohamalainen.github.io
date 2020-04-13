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

Note to self on catching exceptions in multithreaded Haskell code. Literate Haskell source and build scripts and cabal stuff is at <a href="https://github.com/carlohamalainen/playground/tree/master/haskell/exceptions-in-parallel">https://github.com/carlohamalainen/playground/tree/master/haskell/exceptions-in-parallel</a>.

For my use cases there are two scenarios when running a list of worker threads: 

1. If any thread throws an exception, give up on everything. 

2. If any thread throws an exception, log it, but let the other workers run to completion. 

First, imports that we'll use: 

{% highlight haskell %}
{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}

module Main where

import Data.Conduit
import Data.Conduit.List
import Data.Traversable (traverse)
import Control.Applicative
import Control.Monad.Catch
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.ParallelIO.Local
import Control.Monad hiding (mapM, mapM_)
import Control.Monad.Catch
import Data.Typeable
import Prelude hiding (map, mapM, mapM_)
import System.IO
{% endhighlight %}

We will use code from <a href="https://hackage.haskell.org/package/parallel-io">parallel-io</a> and <a href="https://hackage.haskell.org/package/async">async</a> for running worker threads. For a pipeline we'll also use <a href="http://hackage.haskell.org/package/conduit-1.2.4.2/docs/Data-Conduit.html">conduit</a>. 

Here's our exception type, which we throw using ``throwM`` from <a href="https://hackage.haskell.org/package/exceptions-0.8.0.2/docs/Control-Monad-Catch.html">Control.Monad.Catch</a>:

{% highlight haskell %}
data MyException = MyException String deriving (Show, Typeable)

instance Exception MyException
{% endhighlight %}

Our two tasks. The first task immediately throws an exception; the second waits for 5 seconds and completes happily. 

{% highlight haskell %}
task1 :: IO String
task1 = throwM $ MyException "task1 blew up"

task2 :: IO String
task2 = do
  threadDelay $ 5 * 10^6
  return $ "task2 finished"
{% endhighlight %}

## Example: parallel_

{% highlight haskell %}
main1 :: IO ()
main1 = do

  x <- withPool 2 $ \pool -> parallel_ pool [task1, task2]
  print (x :: ())
{% endhighlight %}

Output: 

```
*Main> main1
*** Exception: MyException "task1 blew up"
```

## Example: parallelE_

{% highlight haskell %}
main2 :: IO ()
main2 = do

  x <- withPool 2 $ \pool -> parallelE_ pool [task1, task2]
  print x
{% endhighlight %}

Output: 

```
*Main> main2
[Just (MyException "task1 blew up"),Nothing]
```

## Example: parallel

{% highlight haskell %}
main3 :: IO ()
main3 = do
  x <- withPool 2 $ \pool -> parallel pool [task1, task2]
  print x
{% endhighlight %}

Output: 

```
*Main> main3
*** Exception: MyException "task1 blew up"
```

## Example: parallelE

{% highlight haskell %}
main4 :: IO ()
main4 = do
  x <- withPool 2 $ \pool -> parallelE pool [task1, task2]
  print x
{% endhighlight %}

Output: 

```
*Main> main4
[Left (MyException "task1 blew up"),Right "task2 finished"]
```

## Example: async/wait

{% highlight haskell %}
main5 :: IO ()
main5 = do
  a1 <- async task1
  a2 <- async task2
  result1 <- wait a1
  result2 <- wait a2

  print [result1, result2]
{% endhighlight %}

Output: 

```
*Main> main5
*** Exception: MyException "task1 blew up"
```

## Example: async/waitCatch

{% highlight haskell %}
main6 :: IO ()
main6 = do
  a1 <- async task1
  a2 <- async task2
  result1 <- waitCatch a1
  result2 <- waitCatch a2

  print [result1, result2]
{% endhighlight %}

Output: 

```
*Main> main6
[Left (MyException "task1 blew up"),Right "task2 finished"]
```

## Example: concurrently

{% highlight haskell %}
main7 :: IO ()
main7 = do
  result <- concurrently task1 task2

  print result
{% endhighlight %}

Output: 

```
*Main> main7
*** Exception: MyException "task1 blew up"
```

## Example: throwM in a conduit sink

{% highlight haskell %}
main8 :: IO ()
main8 = do
  sourceList [1..5] $$ (throwM $ MyException "main8 in conduit exploded")
  print "this is never printed"
{% endhighlight %}

Output: 

```
*** Exception: MyException "main8 in conduit exploded"
```

## Example: throwM in a conduit sink (on one value)

{% highlight haskell %}
main9 :: IO ()
main9 = do

  let foo x = if x == 3 then throwM $ MyException "got a 3 in main9"
                        else print x

  sourceList [1..5] $$ (mapM_ foo)
  print "this is never printed"
{% endhighlight %}

The conduit processes values 1 and 2, throws an exception on 3, and never sees 4 and 5. 

```
*Main> main9
1
2
*** Exception: MyException "got a 3 in main9"
```

## Example: throwM/catchC

{% highlight haskell %}
main10 :: IO ()
main10 = do

  let foo x = if x == 3 then throwM $ MyException "got a 3 in main10"
                        else print x

  let sink = catchC (mapM_ foo)
                    (\(e :: SomeException) -> mapM_ $ \x -> putStrLn $ "When processing " ++ show x ++ " caught exception: " ++ show e)

  sourceList [1..5] $$ sink
  print "main10 finished"
{% endhighlight %}

The output is not what I expected. Values 1 and 2 are processed as expected, then the 3 throws an exception, but the effect of ``catchC`` is that **the rest of the values (4 and 5) are processed using the second argument to ``catchC``**. In this situation, a conduit can't be used to process a stream with independently failing components. You have to catch all exceptions before they bubble up to the conduit code. 

```
1
2
When processing 4 caught exception: MyException "got a 3 in main10"
When processing 5 caught exception: MyException "got a 3 in main10"
"main10 finished"
```

## Example: catchAll in conduit

A combinator that runs an IO action and catches any exception: 

{% highlight haskell %}
catchBlah :: Show a => (a -> IO ()) -> a -> IO ()
catchBlah action = \x -> catchAll (action x)
                                  (\(e :: SomeException) -> putStrLn $ "On value " ++ show x ++ " caught exception: " ++ show e)
{% endhighlight %}

Using ``catchBlah`` in the sink: 

{% highlight haskell %}
main11 :: IO ()
main11 = do

  let foo x = if x == 3 then throwM $ MyException "got a 3 in main11"
                        else print x

  sourceList [1..5] $$ (mapM_ $ catchBlah foo)

  print "main11 finished"
{% endhighlight %}

Now the conduit processes every value, because the exception is caught and dealt with at a lower level. 

```
*Main> main11
1
2
On value 3 caught exception: MyException "got a 3 in main11"
4
5
"main11 finished"
```

## Example: catchBlah' in conduit

Now, suppose we have a few stages in the conduit and the first stage blows up. Use ``catchAll`` to catch the exception and return a ``IO (Maybe b)`` instead of ``IO b``: 
{% highlight haskell %}
catchBlah' :: Show a => (a -> IO b) -> a -> IO (Maybe b)
catchBlah' action = \x -> do
  catchAll (action x >>= (return . Just))
           (\(e :: SomeException) -> do putStrLn $ "On value " ++ show x ++ " caught exception: " ++ show e
                                        return Nothing)

main12 :: IO ()
main12 = do

  let src = [1..5] :: [Int]

  let stage1 x = do when (x == 3) $ throwM $ MyException "Got a 3 in stage1"
                    putStrLn $ "First print: " ++ show x
                    return x

  sourceList src $$ (mapM $ catchBlah' stage1) =$= (mapM_ print)

  print "main12 finished"
{% endhighlight %}

Output: 

```
First print: 1
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
```

## Example: catchBlah' in conduit (tweaked)

Same as the previous example but with nicer printing in the sink: 

{% highlight haskell %}
main13 :: IO ()
main13 = do

  let src = [1..5] :: [Int]

  let stage1 x = do when (x == 3) $ throwM $ MyException "Got a 3 in stage1"
                    putStrLn $ "First print: " ++ show x
                    return x
      stage2 x = case x of
                      Just x' -> do putStrLn $ "Second print: " ++ show (x' + 1)
                                    putStrLn ""
                      Nothing -> do putStrLn $ "Second print got Nothing..."
                                    putStrLn ""

  sourceList src $$ (mapM $ catchBlah' stage1) =$= (mapM_ stage2)

  print "main13 finished"
{% endhighlight %}

Output: 

```
*Main> main13
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
```
