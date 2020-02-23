---
id: 763
title: 'Note to self: reader monad transformer'
date: 2014-03-05T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2014/03/05/note-to-self-reader-monad-transformer/
permalink: /2014/03/05/note-to-self-reader-monad-transformer/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
Note to self on constructing a monad transformer. In a way this follows on from the earlier post [Applicatives compose, monads do not](http://carlo-hamalainen.net/blog/2014/1/2/applicatives-compose-monads-do-not). Literate Haskell source for this post is available here: <https://github.com/carlohamalainen/playground/tree/master/haskell/transformers>. 

<pre>&gt; {-# LANGUAGE ScopedTypeVariables, InstanceSigs, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
&gt;
&gt; module MyOwnReaderT where
&gt;
&gt; import Control.Monad
&gt;
&gt; hole = undefined
&gt; data Hole = Hole
</pre>

## The Reader Monad 

A Reader is a data type that encapsulates an environment. The runReader function takes an environment (a Reader) and runs it, producing the result of type a.

<pre>&gt; newtype Reader r a = Reader { runReader :: r -&gt; a }
</pre>

Note that with record syntax, the type of runReader is actually 

<pre>runReader :: Reader r a -&gt; (r -&gt; a)
</pre>

Reader that increments its environment value, returning the same type: 

<pre>&gt; reader1 :: Num r =&gt; Reader r r
&gt; reader1 = Reader $ r -&gt; r + 1
</pre>

Reader that converts its environment value into a string: 

<pre>&gt; reader2 :: Show r =&gt; Reader r String
&gt; reader2 = Reader $ r -&gt; "reader2: " ++ (show r)
</pre>

&#8220;Run&#8221; the reader using runReader: 

<pre>&gt; ghci&gt; runReader reader1 100
&gt; 101
&gt; ghci&gt; runReader reader2 100
&gt; "reader2: 100"
</pre>

There&#8217;s nothing magic about runReader; it is just taking the function out of the data type. We can do it manually ourselves: 

<pre>&gt; runReader' :: Reader r a -&gt; (r -&gt; a)
&gt; runReader' (Reader f) = f
</pre>

<pre>&gt; ghci&gt; runReader' reader1 100
&gt; 101
</pre>

Next, make Reader an instance of Monad: 

<pre>&gt; instance Monad (Reader r) where
&gt;     return :: a -&gt; Reader r a
&gt;     return a = Reader $ _ -&gt; a
&gt;
&gt;     (&gt;&gt;=) :: forall a b. Reader r a -&gt; (a -&gt; Reader r b) -&gt; Reader r b
&gt;     m &gt;&gt;= k  = Reader $ r -&gt; runReader (k (runReader m r :: a) :: Reader r b) r
</pre>

The definition of >>= is relatively easy to work out using [hole driven development](http://matthew.brecknell.net/post/hole-driven-haskell/).

Example usage: 

<pre>&gt; eg1 :: Reader Int String
&gt; eg1 = Reader id &gt;&gt;= e -&gt; return $ "hey, " ++ show e
</pre>

Or in the more readable do notation: 

<pre>&gt; eg1' :: Reader Int String
&gt; eg1' = do e            return $ "hey, " ++ show e
</pre>

<pre>&gt; ghci&gt; runReader eg1' 100
&gt; "hey, 100"
</pre>

Note that we use id to produce a Reader that just passes its environment argument along as the output. See readerAsk later for the equivalent situation which uses return for MonadReader.

Since [] is an instance of Monad we can also do things like this: 

<pre>&gt; eg1'' :: Reader Int String
&gt; eg1'' = do e  [] Int)
&gt;            return $ "hey, " ++ show e
</pre>

<pre>&gt; ghci&gt; runReader eg1'' 100
&gt; "hey, [100]"
</pre>

## Reader Transformer 

We’d like to use the Reader in conjunction with other monads, for example running IO actions but having access to the reader’s environment. To do this we create a transformer, which we’ll call ReaderT:

<pre>&gt; newtype ReaderT r m a = ReaderT { runReaderT :: r -&gt; m a }
</pre>

The r parameter is the reader environment as before, m is the monad (for example IO), and a is the result type, as before. Again, note the actual type of runReaderT:

<pre>&gt; runReaderT :: ReaderT r m a -&gt; (r -&gt; m a)
</pre>

It takes a ReaderT and provides us with a function that takes a reader environment of type r and produces a monadic value. Following from the Monad instance declaration for Reader it is straightforward to write the definition for ReaderT.

<pre>&gt; instance Monad m =&gt; Monad (ReaderT r m) where
&gt;     return a = ReaderT $ r -&gt; return a
&gt;
&gt;     (&gt;&gt;=) :: forall a b. ReaderT r m a -&gt; (a -&gt; ReaderT r m b) -&gt; ReaderT r m b
&gt;     m &gt;&gt;= k  = ReaderT $ r -&gt; do let a' = runReaderT m r :: m a
&gt;                                   a''                                    runReaderT (k a'') r :: m b
</pre>

With a ReaderT as the outer monad, we would like to “lift” a monadic computation into the reader. A monadic action doesn’t know anything about the reader environment, so to lift the monadic value we just create a ReaderT with a function that ignores the environment:

<pre>&gt; liftReaderT :: m a -&gt; ReaderT r m a
&gt; liftReaderT m = ReaderT (_ -&gt; m)
</pre>

Example usage: 

<pre>&gt; egLift :: ReaderT Int IO ()
&gt; egLift = do e  IO Int) -- This is similar to "Reader id" earlier.
&gt;             liftReaderT $ print "boo"
&gt;             liftReaderT $ print $ "value of e: " ++ show e
</pre>

Note the type of return on the first line of egLift. In this context, return :: a -> m a is the equivalent of id :: a -> a from the earlier Reader example.

<pre>&gt; ghci&gt; runReaderT egLift 100
&gt; "boo"
&gt; "value of e: 100"
</pre>

More generally, let’s name the &#8220;ask&#8221; function: 

<pre>&gt; readerAsk :: (Monad m) =&gt; ReaderT r m r
&gt; readerAsk = ReaderT return
</pre>

If we want to modify the environment, we use withReaderT which takes as its first parameter a function to modify the environment. Note that the result is of type ReaderT r’ m a so the function is of type r’ -> r which modifies the supplied reader of type ReaderT r m a. 

<pre>&gt; withReaderT :: (r' -&gt; r) -&gt; ReaderT r m a -&gt; ReaderT r' m a
&gt; withReaderT f rt = ReaderT $ (runReaderT rt) . f
</pre>

Lastly, it is convenient to apply a function to the current environment. 

<pre>&gt; readerReader :: Monad m =&gt; (r -&gt; a) -&gt; ReaderT r m a
&gt; readerReader f = ReaderT $ r -&gt; return (f r)
</pre>

This is almost the same as readerAsk except that we create a reader that returns f r instead of f. In other words: 

<pre>&gt; readerAsk' :: (Monad m) =&gt; ReaderT r m r
&gt; readerAsk' = readerReader id
</pre>

Finally, we collect the functions readerAsk, withReader, and readerReader in a type class MonadReader and give them more general names: 

<pre>&gt; class (Monad m) =&gt; MonadReader r m | m -&gt; r where
&gt;     -- Retrieve the monad environment.
&gt;     ask   :: m r
&gt;
&gt;     -- Execute the computation in the modified environment.
&gt;     local :: (r -&gt; r) -&gt; m a -&gt; m a
&gt;
&gt;     -- Retrieves a function of the current environment.
&gt;     reader :: (r -&gt; a) -&gt; m a
</pre>

An instance declaration for our ReaderT type: 

<pre>&gt; instance Monad m =&gt; MonadReader r (ReaderT r m) where
&gt;     ask    = readerAsk    :: ReaderT r m r
&gt;     local  = withReaderT  :: (r -&gt; r) -&gt; ReaderT r m a -&gt; ReaderT r m a
&gt;     reader = readerReader :: (r -&gt; a) -&gt; ReaderT r m a
</pre>

Now we can write fairly succinct code as follows. Use the IO monad as the inner monad in a ReaderT, with an Int environment and String result type.

<pre>&gt; eg2 :: ReaderT Int IO String
&gt; eg2 = do
&gt;     -- Get the reader environment.
&gt;     e 
&gt;     -- Run an IO action; we have to use liftReaderT since we are currently
&gt;     -- in the ReaderT monad, not the IO monad.
&gt;     liftReaderT $ print $ "I'm in the eg2 function and the environment is: " ++ (show e)
&gt;
&gt;     -- Final return value, a string.
&gt;     return $ "returned value: " ++ show e
</pre>

<pre>&gt; ghci&gt; result  "I'm in the eg2 function and the environment is: 100"
&gt;
&gt; ghci&gt; result
&gt; "returned value: 100"
</pre>

All of the above is available from [Control.Monad.Reader](http://hackage.haskell.org/package/mtl-2.1.2/docs/Control-Monad-Reader.html) and [Control.Monad.Trans](http://hackage.haskell.org/package/mtl-2.1.2/docs/Control-Monad-Trans.html). 

## StateT, ReaderT, and ask 

The [State](http://www.haskell.org/haskellwiki/State_Monad) monad encapsulates a modifiable state. It has a transformer StateT as one would expect. Yet we are able to call ask inside a StateT monad. Here is an example (the raw code is [here](https://github.com/carlohamalainen/playground/blob/master/haskell/transformers/Ask.hs)): 

<pre>&gt; import Control.Monad.Reader
&gt; import Control.Monad.State
&gt;
&gt; inside0 :: ReaderT String IO Float
&gt; inside0 = do
&gt;     e 
&gt;     liftIO $ putStrLn $ "inside0, e: " ++ show e
&gt;
&gt;     return 1.23
&gt;
&gt; inside1 :: StateT [Int] (ReaderT String IO) Float
&gt; inside1 = do
&gt;     e      s 
&gt;     liftIO $ putStrLn $ "inside1, e: " ++ show e
&gt;     liftIO $ putStrLn $ "inside1, s: " ++ show s
&gt;
&gt;     put [1, 1, 1]
&gt;
&gt;     return 1.23
&gt;
&gt; run0 :: IO ()
&gt; run0 = do let layer1 = runReaderT inside0 "reader environment, hi"
&gt;
&gt;           result 
&gt;           print $ "result: " ++ show result
&gt;
&gt; run1 :: IO ()
&gt; run1 = do let layer1 = runStateT inside1 [0]
&gt;               layer2 = runReaderT layer1 "reader environment, hi"
&gt;
&gt;           (result, finalState) 
&gt;           print $ "final state: " ++ show finalState
&gt;
&gt;           print $ "result: " ++ show result
</pre>

The function inside0 has the IO monad nested inside a Reader, while inside1 has a StateT with the ReaderT inside. Yet in both we can write e <- ask. 

Inspecting the types using [ghcmod-vim](https://github.com/eagletmt/ghcmod-vim) we find that 

<pre>&gt;   -- in inside0
&gt;   e 
&gt;   -- in inside1
&gt;   e &lt;- ask :: StateT [Int] (ReaderT String IO) String
</pre>

so there must be a type class that provides the ask function for StateT.

First, inspect the type of ask using ghci (here we are using the definitions from Control.Monad.Reader and Control.Monad.State, not the implementation in this file).

<pre>ghci&gt; :t ask
ask :: MonadReader r m =&gt; m r
</pre>

So StateT must have a MonadReader instance. Confirm this with :i: 

<pre>ghci&gt; :i StateT
(lots of stuff)
instance MonadReader r m =&gt; MonadReader r (StateT s m)
  -- Defined in `Control.Monad.Reader.Class'
(lots of stuff)
</pre>

Looking in [Control.Monad.Reader.Class](http://hackage.haskell.org/package/mtl-2.1.2/docs/Control-Monad-Reader-Class.html) we find: 

<pre>&gt; instance MonadReader r m =&gt; MonadReader r (Lazy.StateT s m) where
&gt;     ask   = lift ask
&gt;     local = Lazy.mapStateT . local
&gt;     reader = lift . reader
</pre>

The lift function comes from [Monad.Trans.Class](https://hackage.haskell.org/package/transformers-0.3.0.0/docs/Control-Monad-Trans-Class.html), and looking there we see: 

<pre>&gt; class MonadTrans t where
&gt;     -- | Lift a computation from the argument monad to the constructed monad.
&gt;     lift :: Monad m =&gt; m a -&gt; t m a
</pre>

So actually we are after the MonadTrans type class. Again looking at :i on StateT we see: 

<pre>ghci&gt; :i StateT
instance MonadTrans (StateT s)
(lots of stuff)
  -- Defined in `Control.Monad.Trans.State.Lazy'
(lots of stuff)
</pre>

So off we go to [Control.Monad.Trans.State.Lazy](https://hackage.haskell.org/package/transformers-0.3.0.0/docs/Control-Monad-Trans-State-Lazy.html) where we finally get the answer: 

<pre>&gt; instance MonadTrans (StateT s) where
&gt;     lift m = StateT $ s -&gt; do
&gt;         a          return (a, s)
</pre>

This shows that for StateT, the lift function takes a monadic action and produces a state transformer that takes the current state, runs the action, and returns the result of the action along with the unmodified state. This makes sense in that the underlying action should not modify the state. (There are some [laws](https://hackage.haskell.org/package/transformers-0.3.0.0/docs/Control-Monad-Trans-Class.html) that monad transformers must satisfy.) 

If we did not have the MonadTrans type class then we would have to embed the ask call manually: 

<pre>&gt; inside1' :: StateT [Int] (ReaderT String IO) Float
&gt; inside1' = do
&gt;     e  do a                             return (a, s)
&gt;
&gt;     s 
&gt;     liftIO $ putStrLn $ "inside1, e: " ++ show e
&gt;     liftIO $ putStrLn $ "inside1, s: " ++ show s
&gt;
&gt;     put [1, 1, 1]
&gt;
&gt;   return 1.23
</pre>

Obviously this is laborious and error-prone. In this case, Haskell’s type class system lets us implement a few classes so that ask, get, put, etc, can be used seamlessly no matter which monad transformer we are in. 

The downside is that reading Haskell code can be nontrivial. In our case we had to follow a trail through a few files to see where ask was actually implemented, and finding the right definition relied on us being able to infer the correct types of certain sub-expressions. 

Personally I am finding more and more that plain vim and ghci does not cut it for Haskell development, and something richer like [ghcmod-vim](https://github.com/eagletmt/ghcmod-vim) is a real necessity. Shameless self plug: [ghc-imported-from](https://github.com/carlohamalainen/ghc-imported-from) is also very useful 🙂 

**Archived Comments**

Date: 2015-07-17 16:38:33.037482 UTC

Author: Marco Faustinellli

Quite an interesting page. My compliments. 

Here&#8217;s my question to you: would you say that the ReaderT must always be the outer layer of the transformers&#8217; stack? 

As a learning exercise I have implemented a transformers library  
(<https://github.com/Muzietto/transformerz>) and I am experimenting  
with the mutual position of ErrorT and ReaderT (which in my case  
are called respectively ET and RT). As you can see at row 122 of  
<https://github.com/Muzietto/transformerz/blob/master/haskell/nilsson/Nilsson_03.hs>  
moving ReaderT inside the monad stack causes trouble with the  
lifting of local (really ugly code with loss of error control  
functionalities). 

I wonder whether ReaderT is free to move, because  
all examples I know put it always at the outer layer. 

Date: 2015-07-18 10:58:58.401802 UTC

Author: Carlo

Hi Marco, 

You&#8217;re right in that it doesn&#8217;t really matter where the Reader  
goes, but it changes how the rest of the stack behaves as  
you noticed with error handling. Here&#8217;s a small example based on your file [Nilsson_03.hs](https://github.com/Muzietto/transformerz/blob/master/haskell/nilsson/Nilsson_03.hs#L122-L123): 

(<https://github.com/carlohamalainen/playground/blob/master/haskell/classy-mtl/Main.hs>) 

<pre>module Main where

import Control.Monad.Except
import Control.Monad.Reader

-- IO then Except then Reader:
type IoExceptReader = ReaderT Int (ExceptT String IO)

dostuff :: ReaderT Int (ExceptT String IO) Int
dostuff = do
    -- Can use ask here (ReaderT):
    r &lt;- ask
    if r == 42
        -- Can also use throwError here (ExceptT)
        then throwError "Got 42, oh no"
        else return $ r + 1000
</pre>

In dostuff we can use ask and throwError, and using local is easy because the Reader is at the top level: 

<pre>blah :: IoExceptReader Int
blah = local (+1) dostuff
</pre>

In contrast, if we put the Except at the top level then the inner computation readerThing runs in a Reader with  
no Except capabilities, while the top level blah&#8217; has Reader and Except. 

<pre>-- IO then Reader then Error:
type IoReaderError = ExceptT String (ReaderT Int IO)

readerThing :: ReaderT Int IO Int
readerThing = do
    -- Can only use ask here, no ExceptT stuff, unlike dostuff.
    r  ExceptT String (ReaderT Int IO) Int
    thisLift = lift
</pre>

Using each version: 

<pre>main :: IO ()
main = do
    e  putStrLn $ "Error!? " ++ err
        Right x     -&gt; putStrLn $ "Got value: " ++ show x

    e'  putStrLn $ "Error!? " ++ err'
        Right x'    -&gt; putStrLn $ "Got value: " ++ show x'

    print ()
</pre>

See also this StackOverflow answer: <http://stackoverflow.com/questions/5075621/what-is-the-difference-between-different-orderings-of-the-same-monad-transformer/5076096#5076096> 

Date: 2015-09-14 18:06:36.589062 UTC

Author: Tapio Niemelä

Great blog! I try to understand the Reader monad. As I&#8217;m noob with haskell I try to play with it. For some reason I cannot compile part (and onwards) where Reader implements Monad. Can you please help me?