---
author: Carlo Hamalainen

date: "2015-05-18T00:00:00Z"
format: image
guid: http://carlo-hamalainen.net/2015/05/18/note-to-self-parallel-io/
id: 823
original_post_id:
- "16"
restapi_import_id:
- 596a05ef0330b
title: 'Note to self: parallel-io'
url: /2015/05/18/note-to-self-parallel-io/
---

A short note about using [parallel-io](https://hackage.haskell.org/package/parallel-io) to run shell commands in parallel from Haskell. If you want to try out this blog post's Literate Haskell source then your best bet is to compile in a sandbox which has various package versions fixed using the ``cabal.config`` file (via the ``cabal freeze`` command).

This is how to build the sandbox:

```
git clone https://github.com/carlohamalainen/playground.git
cd playground/haskell/parallel
rm -fr .cabal-sandbox cabal.sandbox.config dist # start fresh
cabal sandbox init
cabal install --haddock-hyperlink-source --dependencies-only
cabal install
cabal repl
```

Also, note the line

```
  ghc-options:         -threaded -rtsopts -with-rtsopts=-N
```

in ``parallel.cabal``. Without those ``rtsopts`` options you would have to execute the binary using ``./P +RTS -N``.

Now, onto the actual blog post. First, a few imports to get us going.

```haskell
module Main where

import           Control.Concurrent
import           Control.Monad
import           Control.Concurrent.ParallelIO.Local
import           Data.Traversable
import qualified Pipes.ByteString                           as B
import qualified Data.ByteString                            as BS
import qualified Data.ByteString.Lazy                       as BSL
import           Data.ByteString.Internal (w2c)
import           System.Exit
import           System.IO
import           System.Process.Streaming
```

In [one of my work projects](https://github.com/carlohamalainen/imagetrove-uploader) I often need to call legacy command line tools to process various imaging formats (DICOM, MINC, Nifti, etc). I used to use a plain call to ``createProcess`` and then ``readRestOfHandle`` to read the stdout and stderr but I [discovered that it can deadlock](/2014/08/28/reading-stdout-and-stderr-of-createprocess) and a better approach is to use [process-streaming](http://hackage.haskell.org/package/process-streaming).

This is the current snippet that I use:

```haskell
-- Copied from https://github.com/carlohamalainen/imagetrove-uploader/blob/master/src/Network/ImageTrove/Utils.hs
-- Run a shell command, returning Right with stdout if the command exited successfully
-- and Left with stderr if there was an exit failure.
runShellCommand :: FilePath -> [String] -> IO (Either String String)
runShellCommand cmd args = do

    (exitCode, (stdOut, stdErr)) <- execute (pipeoe (fromFold B.toLazyM) (fromFold B.toLazyM)) ((proc cmd args))

    return $ case exitCode of
        ExitSuccess   -> Right $ map w2c $ BS.unpack $ BSL.toStrict stdOut
        ExitFailure e -> Left $ "runShellCommand: exit status " ++ show e ++ " with stdErr: "
                                                                ++ (map w2c $ BS.unpack $ BSL.toStrict $ stdErr)
```

Suppose we have a shell command that takes a while, in this case because it's sleeping. Pretend that it's IO bound.

```haskell
longShellCommand :: Int -> IO (Either String String)
longShellCommand n = do
  putStrLn $ "Running sleep command for " ++ show n ++ " second(s)."
  runShellCommand "sleep" [show n ++ "s"]
```

We could run them in order:

```haskell
main1 :: IO ()
main1 = do
  -- Think of these as arguments to our long-running commands.
  let sleepTimes = [1, 1, 1, 1]

  forM_ sleepTimes longShellCommand
```

In Haskell we can think of ``IO`` as a data type that describes an IO action, so we can build it up using 'pure' code and then execute them later. To make it a bit more explicit, here is a function for running an IO action:

```haskell
runIO :: IO a -> IO a
runIO x = do
  result <- x
  return result
```

We can use it like this:

```
*Main> let action = print 3 -- pure code, nothing happens yet
*Main> runIO action         -- runs the action
3
```

And we can rewrite ``main1`` like this:

```haskell
main2 :: IO ()
main2 = do
  let sleepTimes = [1, 1, 1, 1]

  let actions = map longShellCommand sleepTimes :: [IO (Either String String)]

  forM_ actions runIO
```

As an aside, ``runIO`` is equivalent to ``liftM id`` (see [Control.Monad](http://hackage.haskell.org/package/base-4.8.0.0/docs/Control-Monad.html#v:liftM) for info about ``liftM``).

Now, imagine that you had a lot of these shell commands to execute and wanted a pool of, say, 4 workers.  The [parallel-io](https://hackage.haskell.org/package/parallel-io) package provides ``withPool`` which can be used like this:

```haskell
withPool 4 $ \pool -> parallel_ pool [putStrLn "Echo", putStrLn " in parallel"]
```

Note that the IO actions (the ``putStrLn`` fragments) are provided in a list. A list of IO actions. So we
can run our shell commands in parallel like so:

```haskell
main3 :: IO ()
main3 = do
  let sleepTimes = [1, 1, 1, 1]

  let actions = map longShellCommand sleepTimes :: [IO (Either String String)]

  hSetBuffering stdout LineBuffering -- Otherwise output is garbled.

  withPool 4 $ \pool -> parallel_ pool actions
```

If we did this a lot we might define our own version of ``forM_`` that uses ``withPool``:

```haskell
parForM_ :: Int -> [IO a] -> IO ()
parForM_ nrWorkers tasks = withPool nrWorkers $ \pool -> parallel_ pool tasks

main4 :: IO ()
main4 = do
  let sleepTimes = [1, 1, 1, 1]

  let actions = map longShellCommand sleepTimes :: [IO (Either String String)]

  hSetBuffering stdout LineBuffering -- Otherwise output is garbled.

  parForM_ 4 actions
```

Here is another example of building up some IO actions in pure form and then executing them later. Imagine that instead of a list of Ints for the sleep times, we have some actual sleep times and others that represent an error case.  An easy way to model this is using [Either](http://hackage.haskell.org/package/base-4.8.0.0/docs/Data-Either.html#t:Either), which by convention has the erroneous values in the ``Left`` and correct values in the ``Right``.

```haskell
main5 :: IO ()
main5 = do
  let sleepTimes = [Right 1, Left "something went wrong", Right 2, Right 3]

  let actions  = map (traverse longShellCommand) sleepTimes :: [IO (Either [Char] (Either String String))]
      actions' = map (fmap join) actions                    :: [IO (Either [Char] String)]

  hSetBuffering stdout LineBuffering -- Otherwise output is garbled.

  parForM_ 4 actions'
```

In ``main5`` we define ``actions`` by mapping a function over the sleep times, which are are now of type ``Either String Int``. We can't apply ``longShellCommand`` directly because it expects an ``Int``, so we use ``traverse longShellCommand`` instead (see [Data.Traversable](http://hackage.haskell.org/package/base-4.8.0.0/docs/Data-Traversable.html#v:traverse) for the definition of ``traverse``).

Next, the Either-of-Either is a bit clunky but we can mash them together using ``join``. Here we have
to use ``fmap`` because we have list elements of type ``IO (Either [Char] String)``, not ``Either [Char] String`` as ``join`` might expect.

One topic that I haven't touched on is dealing with asynchronous exceptions. For this, have a read of [Catching all exceptions](https://www.fpcomplete.com/user/snoyberg/general-haskell/exceptions/catching-all-exceptions) from Snoyman and also [enclosed-exceptions](http://hackage.haskell.org/package/enclosed-exceptions). Also, [Chapter 13](http://chimera.labs.oreilly.com/books/1230000000929/ch13.html) of [Parallel and Concurrent Programming in Haskell](http://chimera.labs.oreilly.com/books/1230000000929) shows how to use the handy [async](https://hackage.haskell.org/package/async) package.

```haskell
-- Run all of the mains.
main :: IO ()
main = do
  print "main1"
  main1

  print "main2"
  main2

  print "main3"
  main3

  print "main4"
  main4

  print "main5"
  main5
```
