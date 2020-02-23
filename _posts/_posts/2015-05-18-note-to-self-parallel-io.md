---
id: 823
title: 'Note to self: parallel-io'
date: 2015-05-18T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2015/05/18/note-to-self-parallel-io/
permalink: /2015/05/18/note-to-self-parallel-io/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
A short note about using [parallel-io](https://hackage.haskell.org/package/parallel-io) to run shell commands in parallel from Haskell. If you want to try out this blog post’s Literate Haskell source then your best bet is to compile in a sandbox which has various package versions fixed using the cabal.config file (via the cabal freeze command). 

This is how to build the sandbox: 

<pre>git clone https://github.com/carlohamalainen/playground.git
cd playground/haskell/parallel
rm -fr .cabal-sandbox cabal.sandbox.config dist # start fresh
cabal sandbox init
cabal install --haddock-hyperlink-source --dependencies-only
cabal install
cabal repl
</pre>

Also, note the line 

<pre>ghc-options:         -threaded -rtsopts -with-rtsopts=-N
</pre>

in parallel.cabal. Without those rtsopts options you would have to execute the binary using ./P +RTS -N. 

Now, onto the actual blog post. First, a few imports to get us going. 

<pre>&gt; module Main where
</pre>

<pre>&gt; import           Control.Concurrent
&gt; import           Control.Monad
&gt; import           Control.Concurrent.ParallelIO.Local
&gt; import           Data.Traversable
&gt; import qualified Pipes.ByteString                           as B
&gt; import qualified Data.ByteString                            as BS
&gt; import qualified Data.ByteString.Lazy                       as BSL
&gt; import           Data.ByteString.Internal (w2c)
&gt; import           System.Exit
&gt; import           System.IO
&gt; import           System.Process.Streaming
</pre>

In [one of my work projects](https://github.com/carlohamalainen/imagetrove-uploader) I often need to call legacy command line tools to process various imaging formats (DICOM, MINC, Nifti, etc). I used to use a plain call to createProcess and then readRestOfHandle to read the stdout and stderr but I [discovered that it can deadlock](http://carlo-hamalainen.net/blog/2014/8/28/reading-stdout-and-stderr-of-createprocess) and a better approach is to use [process-streaming](http://hackage.haskell.org/package/process-streaming). This is the current snippet that I use: 

<pre>&gt; -- Copied from https://github.com/carlohamalainen/imagetrove-uploader/blob/master/src/Network/ImageTrove/Utils.hs
&gt; -- Run a shell command, returning Right with stdout if the command exited successfully
&gt; -- and Left with stderr if there was an exit failure.
&gt; runShellCommand :: FilePath -&gt; [String] -&gt; IO (Either String String)
&gt; runShellCommand cmd args = do
&gt;
&gt;     (exitCode, (stdOut, stdErr)) 
&gt;     return $ case exitCode of
&gt;         ExitSuccess   -&gt; Right $ map w2c $ BS.unpack $ BSL.toStrict stdOut
&gt;         ExitFailure e -&gt; Left $ "runShellCommand: exit status " ++ show e ++ " with stdErr: "
&gt;                                                                 ++ (map w2c $ BS.unpack $ BSL.toStrict $ stdErr)
</pre>

Suppose we have a shell command that takes a while, in this case because it’s sleeping. Pretend that it’s IO bound. 

<pre>&gt; longShellCommand :: Int -&gt; IO (Either String String)
&gt; longShellCommand n = do
&gt;   putStrLn $ "Running sleep command for " ++ show n ++ " second(s)."
&gt;   runShellCommand "sleep" [show n ++ "s"]
</pre>

We could run them in order: 

<pre>&gt; main1 :: IO ()
&gt; main1 = do
&gt;   -- Think of these as arguments to our long-running commands.
&gt;   let sleepTimes = [1, 1, 1, 1]
&gt;
&gt;   forM_ sleepTimes longShellCommand
</pre>

In Haskell we can think of IO as a data type that describes an IO action, so we can build it up using ‘pure’ code and then execute them later. To make it a bit more explicit, here is a function for running an IO action: 

<pre>&gt; runIO :: IO a -&gt; IO a
&gt; runIO x = do
&gt;   result    return result
</pre>

We can use it like this: 

<pre>*Main&gt; let action = print 3 -- pure code, nothing happens yet
*Main&gt; runIO action         -- runs the action
3
</pre>

And we can rewrite main1 like this: 

<pre>&gt; main2 :: IO ()
&gt; main2 = do
&gt;   let sleepTimes = [1, 1, 1, 1]
&gt;
&gt;   let actions = map longShellCommand sleepTimes :: [IO (Either String String)]
&gt;
&gt;   forM_ actions runIO
</pre>

As an aside, runIO is equivalent to liftM id (see [Control.Monad](http://hackage.haskell.org/package/base-4.8.0.0/docs/Control-Monad.html#v:liftM) for info about liftM). 

Now, imagine that you had a lot of these shell commands to execute and wanted a pool of, say, 4 workers. The [parallel-io](https://hackage.haskell.org/package/parallel-io) package provides withPool which can be used like this: 

<pre>withPool 4 $ pool -&gt; parallel_ pool [putStrLn "Echo", putStrLn " in parallel"]
</pre>

Note that the IO actions (the putStrLn fragments) are provided in a list. A list of IO actions. So we can run our shell commands in parallel like so: 

<pre>&gt; main3 :: IO ()
&gt; main3 = do
&gt;   let sleepTimes = [1, 1, 1, 1]
&gt;
&gt;   let actions = map longShellCommand sleepTimes :: [IO (Either String String)]
&gt;
&gt;   hSetBuffering stdout LineBuffering -- Otherwise output is garbled.
&gt;
&gt;   withPool 4 $ pool -&gt; parallel_ pool actions
</pre>

If we did this a lot we might define our own version of forM_ that uses withPool: 

<pre>&gt; parForM_ :: Int -&gt; [IO a] -&gt; IO ()
&gt; parForM_ nrWorkers tasks = withPool nrWorkers $ pool -&gt; parallel_ pool tasks
</pre>

<pre>&gt; main4 :: IO ()
&gt; main4 = do
&gt;   let sleepTimes = [1, 1, 1, 1]
&gt;
&gt;   let actions = map longShellCommand sleepTimes :: [IO (Either String String)]
&gt;
&gt;   hSetBuffering stdout LineBuffering -- Otherwise output is garbled.
&gt;
&gt;   parForM_ 4 actions
</pre>

Here is another example of building up some IO actions in pure form and then executing them later. Imagine that instead of a list of Ints for the sleep times, we have some actual sleep times and others that represent an error case. An easy way to model this is using [Either](http://hackage.haskell.org/package/base-4.8.0.0/docs/Data-Either.html#t:Either), which by convention has the erroneous values in the Left and correct values in the Right. 

<pre>&gt; main5 :: IO ()
&gt; main5 = do
&gt;   let sleepTimes = [Right 1, Left "something went wrong", Right 2, Right 3]
&gt;
&gt;   let actions  = map (traverse longShellCommand) sleepTimes :: [IO (Either [Char] (Either String String))]
&gt;       actions' = map (fmap join) actions                    :: [IO (Either [Char] String)]
&gt;
&gt;   hSetBuffering stdout LineBuffering -- Otherwise output is garbled.
&gt;
&gt;   parForM_ 4 actions'
</pre>

In main5 we define actions by mapping a function over the sleep times, which are are now of type Either String Int. We can’t apply longShellCommand directly because it expects an Int, so we use traverse longShellCommand instead (see [Data.Traversable](http://hackage.haskell.org/package/base-4.8.0.0/docs/Data-Traversable.html#v:traverse) for the definition of traverse). 

Next, the Either-of-Either is a bit clunky but we can mash them together using join. Here we have to use fmap because we have list elements of type IO (Either [Char] String), not Either [Char] String as join might expect. 

One topic that I haven’t touched on is dealing with asynchronous exceptions. For this, have a read of [Catching all exceptions](https://www.fpcomplete.com/user/snoyberg/general-haskell/exceptions/catching-all-exceptions) from Snoyman and also [enclosed-exceptions](http://hackage.haskell.org/package/enclosed-exceptions). Also, [Chapter 13](http://chimera.labs.oreilly.com/books/1230000000929/ch13.html) of [Parallel and Concurrent Programming in Haskell](http://chimera.labs.oreilly.com/books/1230000000929) shows how to use the handy [async](https://hackage.haskell.org/package/async) package. 

<pre>&gt; -- Run all of the mains.
&gt; main :: IO ()
&gt; main = do
&gt;
&gt;   print "main1"
&gt;   main1
&gt;
&gt;   print "main2"
&gt;   main2
&gt;
&gt;   print "main3"
&gt;   main3
&gt;
&gt;   print "main4"
&gt;   main4
&gt;
&gt;   print "main5"
&gt;   main5
</pre>

**Archived Comments**

Date: 2015-05-26 01:49:57.735584 UTC

Author: Franklin Chen

A side note: \`runShellCommand\` does not seem to play well with Unicode, because it does not do decoding of bytes, e.g.,

> runShellCommand &#8220;ls&#8221; [&#8220;écoutez&#8221;]  
Left &#8220;runShellCommand: exit status 1 with stdErr: ls: 195169coutez: No such file or directoryn&#8221;

Note that I have LANG=en_US.UTF-8 set on my Mac.