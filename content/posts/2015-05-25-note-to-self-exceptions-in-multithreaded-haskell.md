---
author: Carlo Hamalainen

date: "2015-05-25T00:00:00Z"
format: image
title: 'Note to self: exceptions in multithreaded Haskell'
url: /2015/05/25/note-to-self-exceptions-in-multithreaded-haskell/
---

Note to self on catching exceptions in multithreaded Haskell code. Literate Haskell source and build scripts and cabal stuff is at [https://github.com/carlohamalainen/playground/tree/master/haskell/exceptions-in-parallel](https://github.com/carlohamalainen/playground/tree/master/haskell/exceptions-in-parallel).

For my use cases there are two scenarios when running a list of worker threads:

1. If any thread throws an exception, give up on everything.

2. If any thread throws an exception, log it, but let the other workers run to completion.

First, imports that we'll use:

```haskell
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
```

We will use code from [parallel-io](https://hackage.haskell.org/package/parallel-io) and [async](https://hackage.haskell.org/package/async) for running worker threads. For a pipeline we'll also use [conduit](http://hackage.haskell.org/package/conduit-1.2.4.2/docs/Data-Conduit.html).

Here's our exception type, which we throw using ``throwM`` from [Control.Monad.Catch](https://hackage.haskell.org/package/exceptions-0.8.0.2/docs/Control-Monad-Catch.html):

```haskell
data MyException = MyException String deriving (Show, Typeable)

instance Exception MyException
```

Our two tasks. The first task immediately throws an exception; the second waits for 5 seconds and completes happily.

```haskell
task1 :: IO String
task1 = throwM $ MyException "task1 blew up"

task2 :: IO String
task2 = do
  threadDelay $ 5 * 10^6
  return $ "task2 finished"
```

## Example: parallel_

```haskell
main1 :: IO ()
main1 = do

  x <- withPool 2 $ \pool -> parallel_ pool [task1, task2]
  print (x :: ())
```

Output:

```
*Main> main1
*** Exception: MyException "task1 blew up"
```

## Example: parallelE_

```haskell
main2 :: IO ()
main2 = do

  x <- withPool 2 $ \pool -> parallelE_ pool [task1, task2]
  print x
```

Output:

```
*Main> main2
[Just (MyException "task1 blew up"),Nothing]
```

## Example: parallel

```haskell
main3 :: IO ()
main3 = do
  x <- withPool 2 $ \pool -> parallel pool [task1, task2]
  print x
```

Output:

```
*Main> main3
*** Exception: MyException "task1 blew up"
```

## Example: parallelE

```haskell
main4 :: IO ()
main4 = do
  x <- withPool 2 $ \pool -> parallelE pool [task1, task2]
  print x
```

Output:

```
*Main> main4
[Left (MyException "task1 blew up"),Right "task2 finished"]
```

## Example: async/wait

```haskell
main5 :: IO ()
main5 = do
  a1 <- async task1
  a2 <- async task2
  result1 <- wait a1
  result2 <- wait a2

  print [result1, result2]
```

Output:

```
*Main> main5
*** Exception: MyException "task1 blew up"
```

## Example: async/waitCatch

```haskell
main6 :: IO ()
main6 = do
  a1 <- async task1
  a2 <- async task2
  result1 <- waitCatch a1
  result2 <- waitCatch a2

  print [result1, result2]
```

Output:

```
*Main> main6
[Left (MyException "task1 blew up"),Right "task2 finished"]
```

## Example: concurrently

```haskell
main7 :: IO ()
main7 = do
  result <- concurrently task1 task2

  print result
```

Output:

```
*Main> main7
*** Exception: MyException "task1 blew up"
```

## Example: throwM in a conduit sink

```haskell
main8 :: IO ()
main8 = do
  sourceList [1..5] $$ (throwM $ MyException "main8 in conduit exploded")
  print "this is never printed"
```

Output:

```
*** Exception: MyException "main8 in conduit exploded"
```

## Example: throwM in a conduit sink (on one value)

```haskell
main9 :: IO ()
main9 = do

  let foo x = if x == 3 then throwM $ MyException "got a 3 in main9"
                        else print x

  sourceList [1..5] $$ (mapM_ foo)
  print "this is never printed"
```

The conduit processes values 1 and 2, throws an exception on 3, and never sees 4 and 5.

```
*Main> main9
1
2
*** Exception: MyException "got a 3 in main9"
```

## Example: throwM/catchC

```haskell
main10 :: IO ()
main10 = do

  let foo x = if x == 3 then throwM $ MyException "got a 3 in main10"
                        else print x

  let sink = catchC (mapM_ foo)
                    (\(e :: SomeException) -> mapM_ $ \x -> putStrLn $ "When processing " ++ show x ++ " caught exception: " ++ show e)

  sourceList [1..5] $$ sink
  print "main10 finished"
```

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

```haskell
catchBlah :: Show a => (a -> IO ()) -> a -> IO ()
catchBlah action = \x -> catchAll (action x)
                                  (\(e :: SomeException) -> putStrLn $ "On value " ++ show x ++ " caught exception: " ++ show e)
```

Using ``catchBlah`` in the sink:

```haskell
main11 :: IO ()
main11 = do

  let foo x = if x == 3 then throwM $ MyException "got a 3 in main11"
                        else print x

  sourceList [1..5] $$ (mapM_ $ catchBlah foo)

  print "main11 finished"
```

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

```haskell
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
```

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

```haskell
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
```

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
