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

Note to self on constructing a monad transformer. In a way this follows on from the earlier post <a href="http://carlo-hamalainen.net/blog/2014/1/2/applicatives-compose-monads-do-not">Applicatives compose, monads do not</a>. Literate Haskell source for this post is available here: <a href="https://github.com/carlohamalainen/playground/tree/master/haskell/transformers">https://github.com/carlohamalainen/playground/tree/master/haskell/transformers</a>.

{% highlight haskell %}
{-# LANGUAGE ScopedTypeVariables, InstanceSigs, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module MyOwnReaderT where

import Control.Monad

hole = undefined
data Hole = Hole
{% endhighlight %}

## The Reader Monad

A ``Reader`` is a data type that encapsulates an environment. The ``runReader`` function takes an environment (a ``Reader``) and runs it, producing the result of type ``a``.

{% highlight haskell %}
newtype Reader r a = Reader { runReader :: r -> a }
{% endhighlight %}

Note that with record syntax, the type of ``runReader`` is actually 

{% highlight haskell %}
runReader :: Reader r a -> (r -> a)
{% endhighlight %}

Reader that increments its environment value, returning the same type: 

{% highlight haskell %}
reader1 :: Num r => Reader r r
reader1 = Reader $ \r -> r + 1
{% endhighlight %}

Reader that converts its environment value into a string: 

{% highlight haskell %}
reader2 :: Show r => Reader r String
reader2 = Reader $ \r -> "reader2: " ++ (show r)
{% endhighlight %}

"Run" the reader using ``runReader``: 

```
ghci> runReader reader1 100
101
ghci> runReader reader2 100
"reader2: 100"
```

There's nothing magic about ``runReader``; it is just taking the function out
of the data type. We can do it manually ourselves: 

{% highlight haskell %}
runReader' :: Reader r a -> (r -> a)
runReader' (Reader f) = f
{% endhighlight %}

```
ghci> runReader' reader1 100
101
```

Next, make ``Reader`` an instance of ``Monad``: 

{% highlight haskell %}
instance Monad (Reader r) where
    return :: a -> Reader r a
    return a = Reader $ \_ -> a

    (>>=) :: forall a b. Reader r a -> (a -> Reader r b) -> Reader r b
    m >>= k  = Reader $ \r -> runReader (k (runReader m r :: a) :: Reader r b) r
{% endhighlight %}

The definition of ``>>=`` is relatively easy to work out using <a href="http://matthew.brecknell.net/post/hole-driven-haskell/">hole driven development</a>. 

Example usage: 

{% highlight haskell %}
eg1 :: Reader Int String
eg1 = Reader id >>= \e -> return $ "hey, " ++ show e
{% endhighlight %}

Or in the more readable ``do`` notation: 

{% highlight haskell %}
eg1' :: Reader Int String
eg1' = do e <- Reader id
          return $ "hey, " ++ show e
{% endhighlight %}

```
ghci> runReader eg1' 100
"hey, 100"
```

Note that we use ``id`` to produce a ``Reader`` that just passes its environment argument along as the output. See ``readerAsk`` later for the equivalent situation which uses ``return`` for ``MonadReader``. 

Since ``[]`` is an instance of ``Monad`` we can also do things like this: 

{% highlight haskell %}
eg1'' :: Reader Int String
eg1'' = do e <- Reader (return :: Int -> [] Int)
           return $ "hey, " ++ show e
{% endhighlight %}

```
ghci> runReader eg1'' 100
"hey, [100]"
```

## Reader Transformer

We'd like to use the ``Reader`` in conjunction with other monads, for example running ``IO`` actions but
having access to the reader's environment. To do this we create a transformer, which we'll call ``ReaderT``:

{% highlight haskell %}
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
{% endhighlight %}

The ``r`` parameter is the reader environment as before, ``m`` is the monad (for example ``IO``), and ``a`` is the result type, as before. Again, note the actual type of ``runReaderT``: 

{% highlight haskell %}
runReaderT :: ReaderT r m a -> (r -> m a)
{% endhighlight %}

It takes a ``ReaderT`` and provides us with a function that takes a reader environment of type ``r`` and produces a monadic value. Following from the ``Monad`` instance declaration for ``Reader`` it is straightforward to write the definition for ``ReaderT``. 

{% highlight haskell %}
instance Monad m => Monad (ReaderT r m) where
    return a = ReaderT $ \r -> return a

    (>>=) :: forall a b. ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
    m >>= k  = ReaderT $ \r -> do let a' = runReaderT m r :: m a
                                  a'' <- a'
                                  runReaderT (k a'') r :: m b
{% endhighlight %}

With a ``ReaderT`` as the outer monad, we would like to "lift" a monadic computation into the reader. A monadic action doesn't know anything about the reader environment, so to lift the monadic value we just create a ``ReaderT`` with a function that ignores the environment: 

{% highlight haskell %}
liftReaderT :: m a -> ReaderT r m a
liftReaderT m = ReaderT (\_ -> m)
{% endhighlight %}

Example usage: 

{% highlight haskell %}
egLift :: ReaderT Int IO ()
egLift = do e <- ReaderT (return :: Int -> IO Int) -- This is similar to "Reader id" earlier.
            liftReaderT $ print "boo"
            liftReaderT $ print $ "value of e: " ++ show e
{% endhighlight %}

Note the type of ``return`` on the first line of ``egLift``. In this context, ``return :: a -> m a`` is the equivalent of ``id :: a -> a`` from the earlier ``Reader`` example.

```
ghci> runReaderT egLift 100
"boo"
"value of e: 100"
```

More generally, let's name the ``ask`` function: 

{% highlight haskell %}
readerAsk :: (Monad m) => ReaderT r m r
readerAsk = ReaderT return
{% endhighlight %}

If we want to modify the environment, we use ``withReaderT`` which takes as its first parameter a function to modify the environment. Note that the result is of type ``ReaderT r' m a`` so the function is of type ``r' -> r`` which modifies the supplied reader of type ``ReaderT r m a``. 

{% highlight haskell %}
withReaderT :: (r' -> r) -> ReaderT r m a -> ReaderT r' m a
withReaderT f rt = ReaderT $ (runReaderT rt) . f
{% endhighlight %}

Lastly, it is convenient to apply a function to the current environment. 

{% highlight haskell %}
readerReader :: Monad m => (r -> a) -> ReaderT r m a
readerReader f = ReaderT $ \r -> return (f r)
{% endhighlight %}

This is almost the same as ``readerAsk`` except that we create a reader that returns ``f r`` instead of ``f``. In other words: 

{% highlight haskell %}
readerAsk' :: (Monad m) => ReaderT r m r
readerAsk' = readerReader id
{% endhighlight %}

Finally, we collect the functions ``readerAsk``, ``withReader``, and ``readerReader`` in a type class ``MonadReader`` and give them more general names: 

{% highlight haskell %}
class (Monad m) => MonadReader r m | m -> r where
    -- Retrieve the monad environment.
    ask   :: m r

    -- Execute the computation in the modified environment.
    local :: (r -> r) -> m a -> m a

    -- Retrieves a function of the current environment.
    reader :: (r -> a) -> m a
{% endhighlight %}

An instance declaration for our ``ReaderT`` type:

{% highlight haskell %}
instance Monad m => MonadReader r (ReaderT r m) where
    ask    = readerAsk    :: ReaderT r m r
    local  = withReaderT  :: (r -> r) -> ReaderT r m a -> ReaderT r m a
    reader = readerReader :: (r -> a) -> ReaderT r m a
{% endhighlight %}

Now we can write fairly succinct code as follows. Use the ``IO`` monad as the inner monad in a ``ReaderT``, with an ``Int`` environment and ``String`` result type. 

{% highlight haskell %}
eg2 :: ReaderT Int IO String
eg2 = do
    -- Get the reader environment.
    e <- ask :: ReaderT Int IO Int

    -- Run an IO action; we have to use liftReaderT since we are currently
    -- in the ReaderT monad, not the IO monad.
    liftReaderT $ print $ "I'm in the eg2 function and the environment is: " ++ (show e)

    -- Final return value, a string.
    return $ "returned value: " ++ show e
{% endhighlight %}

```
ghci> result <- runReaderT eg2 100
"I'm in the eg2 function and the environment is: 100"

ghci> result
"returned value: 100"
```

All of the above is available from <a href="http://hackage.haskell.org/package/mtl-2.1.2/docs/Control-Monad-Reader.html">Control.Monad.Reader</a> and <a href="http://hackage.haskell.org/package/mtl-2.1.2/docs/Control-Monad-Trans.html">Control.Monad.Trans</a>. 

## StateT, ReaderT, and ask

The <a href="http://www.haskell.org/haskellwiki/State_Monad">State</a> monad encapsulates a modifiable state. It has a transformer ``StateT`` as one would expect. Yet we are able to call ``ask`` inside a ``StateT`` monad. Here is an example (the raw code is <a href="https://github.com/carlohamalainen/playground/blob/master/haskell/transformers/Ask.hs">here</a>): 

{% highlight haskell %}
import Control.Monad.Reader
import Control.Monad.State

inside0 :: ReaderT String IO Float
inside0 = do
    e <- ask :: ReaderT String IO String

    liftIO $ putStrLn $ "inside0, e: " ++ show e

    return 1.23

inside1 :: StateT [Int] (ReaderT String IO) Float
inside1 = do
    e <- ask :: StateT [Int] (ReaderT String IO) String
    s <- get :: StateT [Int] (ReaderT String IO) [Int]

    liftIO $ putStrLn $ "inside1, e: " ++ show e
    liftIO $ putStrLn $ "inside1, s: " ++ show s

    put [1, 1, 1]

    return 1.23

run0 :: IO ()
run0 = do let layer1 = runReaderT inside0 "reader environment, hi"

          result <- layer1

          print $ "result: " ++ show result

run1 :: IO ()
run1 = do let layer1 = runStateT inside1 [0]
              layer2 = runReaderT layer1 "reader environment, hi"

          (result, finalState) <- layer2

          print $ "final state: " ++ show finalState

          print $ "result: " ++ show result
{% endhighlight %}

The function ``inside0`` has the ``IO`` monad nested inside a ``Reader``, while ``inside1`` has a ``StateT`` with the ``ReaderT`` inside. Yet in both we can write ``e <- ask``. 

Inspecting the types using <a href="https://github.com/eagletmt/ghcmod-vim">ghcmod-vim</a> we find that 

{% highlight haskell %}
-- in inside0
e <- ask :: ReaderT String IO String

-- in inside1
e <- ask :: StateT [Int] (ReaderT String IO) String
{% endhighlight %}

so there must be a type class that provides the ``ask`` function for ``StateT``.

First, inspect the type of ``ask`` using ghci (here we are using the definitions from ``Control.Monad.Reader`` and ``Control.Monad.State``, not the implementation in this file).

```
ghci> :t ask
ask :: MonadReader r m => m r
```

So ``StateT`` must have a ``MonadReader`` instance. Confirm this with ``:i``: 

```
ghci> :i StateT
(lots of stuff)
instance MonadReader r m => MonadReader r (StateT s m)
  -- Defined in `Control.Monad.Reader.Class'
(lots of stuff)
```

Looking in <a href="http://hackage.haskell.org/package/mtl-2.1.2/docs/Control-Monad-Reader-Class.html">Control.Monad.Reader.Class</a> we find: 

{% highlight haskell %}
instance MonadReader r m => MonadReader r (Lazy.StateT s m) where
    ask   = lift ask
    local = Lazy.mapStateT . local
    reader = lift . reader
{% endhighlight %}

The ``lift`` function comes from <a href="https://hackage.haskell.org/package/transformers-0.3.0.0/docs/Control-Monad-Trans-Class.html">Monad.Trans.Class</a>,
and looking there we see: 

{% highlight haskell %}
class MonadTrans t where
    -- | Lift a computation from the argument monad to the constructed monad.
    lift :: Monad m => m a -> t m a
{% endhighlight %}

So actually we are after the ``MonaTrans`` type class. Again looking at ``:i`` on ``StateT`` we see: 

```
ghci> :i StateT
instance MonadTrans (StateT s)
(lots of stuff)
  -- Defined in `Control.Monad.Trans.State.Lazy'
(lots of stuff)
```

So off we go to <a href="https://hackage.haskell.org/package/transformers-0.3.0.0/docs/Control-Monad-Trans-State-Lazy.html">Control.Monad.Trans.State.Lazy</a> where we finally get the answer: 

{% highlight haskell %}
instance MonadTrans (StateT s) where
    lift m = StateT $ \s -> do
        a <- m
        return (a, s)
{% endhighlight %}

This shows that for ``StateT``, the ``lift`` function takes a monadic action and produces a state transformer that takes the current state, runs the action, and returns the result of the action along with the unmodified state. This makes sense in that the underlying action should not modify the state. (There are some <a href="https://hackage.haskell.org/package/transformers-0.3.0.0/docs/Control-Monad-Trans-Class.html">laws</a> that monad transformers must satisfy.) 

If we did not have the ``MonadTrans`` type class then we would have to embed the ``ask`` call manually: 

{% highlight haskell %}
inside1' :: StateT [Int] (ReaderT String IO) Float
inside1' = do
    e <- StateT $ \s -> do a <- ask
                           return (a, s)

    s <- get :: StateT [Int] (ReaderT String IO) [Int]

    liftIO $ putStrLn $ "inside1, e: " ++ show e
    liftIO $ putStrLn $ "inside1, s: " ++ show s

    put [1, 1, 1]

    return 1.23
{% endhighlight %}

Obviously this is laborious and error-prone. In this case, Haskell's type class system lets us implement a few classes so that ``ask``, ``get``, ``put``, etc, can be used seamlessly no matter which monad transformer we are in. 

The downside is that reading Haskell code can be nontrivial. In our case we had to follow a trail through a few files to see where ``ask`` was actually implemented, and finding the right definition relied on us being able to infer the correct types of certain sub-expressions. 

Personally I am finding more and more that plain vim and ghci does not cut it for Haskell development, and something richer like <a href="https://github.com/eagletmt/ghcmod-vim">ghcmod-vim</a> is a real necessity. Shameless self plug: <a href="https://github.com/carlohamalainen/ghc-imported-from">ghc-imported-from</a> is also very useful :-) 

