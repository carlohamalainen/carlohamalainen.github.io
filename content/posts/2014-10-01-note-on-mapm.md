---
author: Carlo Hamalainen

date: "2014-10-01T00:00:00Z"
format: image
guid: http://carlo-hamalainen.net/2014/10/01/note-on-mapm/
id: 756
original_post_id:
- "16"
restapi_import_id:
- 596a05ef0330b
title: Note on mapM
url: /2014/10/01/note-on-mapm/
---

Note to self about ``mapM``. Is it lazy? Sort of.

Literate source is here: [https://github.com/carlohamalainen/playground/tree/master/haskell/mapm](https://github.com/carlohamalainen/playground/tree/master/haskell/mapm).

First, some imports:

```haskell
{-# LANGUAGE OverloadedStrings, InstanceSigs #-}

import Control.Applicative
import Control.Monad
import qualified Data.ByteString as B
import Data.ByteString.Internal (w2c)
import Data.Either
```

I recently wrote some code using [wreq](http://hackage.haskell.org/package/wreq)
that seemed to use much more memory than I thought it should. The problem turned out not to be
with wreq but with the way that I was using ``mapM``. An equivalent snippet of code is:

```haskell
main1 = do
  firstFile <- head <$> mapM B.readFile (take 100000 $ repeat "MapM.lhs")
  print $ B.length firstFile
```

I reasoned that ``mapM`` would construct its result lazily, then ``head`` would force evaluation of just the first element of the list. This isn't the case, as [explained here](http://stackoverflow.com/questions/3270255/is-haskells-mapm-not-lazy).  The function ``mapM`` is basically equivalent to this:

```haskell
mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' m [] = return []
mapM' m (x:xs) = do
  x' <- m x
  xs' <- mapM' m xs
  return (x':xs')
```

So the monadic action ``m`` is evaluated to build up the list elements.

One of the answers on the StackOverflow page says to use a step by step series to only evaluate the bits that are required:

```haskell
data Stream m a = Nil | Stream a (m (Stream m a))
```

GHC 7.8.3 comes with [Stream](http://www.haskell.org/ghc/docs/latest/html/libraries/ghc/src/Stream.html) defined as:

```
-- In GHC 7.8.3:
newtype Stream m a b = Stream { runStream :: m (Either b (a, Stream m a b)) }
```

The idea is that it represents a sequence of monadic actions. A ``Left`` is a final value of type ``b``, while ``Right (a, Stream m a b)`` represents an intermediate value of type ``a`` along with the remaining stream.

The ``Monad`` instance is fairly straightforward. The ``return`` function turns a plain value into a final value (hence the ``Left``), and the bind either stops with the final value or produces the new value along with the next stream.

```haskell
instance Monad m => Monad (Stream m a) where
  return a = Stream $ return $ Left a
  Stream m >>= k = Stream $ do
                      r <- m
                      case r of
                          Left b         -> runStream $ k b
                          Right (a, str) -> return $ Right (a, str >>= k)
```

There are also instances for ``Functor`` and ``Applicative`` but we don't need them here.

A handy function is ``liftIO`` which turns a normal monadic action into a stream:

```haskell
liftIO :: IO a -> Stream IO b a
liftIO io = Stream $ io >>= return . Left
```

It just runs the ``io`` action, and pipes it to a ``Left`` and then returns it in a ``Stream``.

```haskell
readFileS :: FilePath -> Stream IO b B.ByteString
readFileS f = liftIO $ B.readFile f
```

To use ``readFileS`` we wrap it with ``runStream``:

```
*Main> Left x <- runStream $ readFileS "MapM.lhs"
*Main> print $ B.length x
4243
```

So we can produce final values, but what about intermediate ones? This is what ``yield`` does:

```haskell
yield :: Monad m => a -> Stream m a ()
yield a = Stream $ return $ Right $ (a, return ())
```

At this point we have no idea about the remaining stream, so we return the unit ``()``.

For testing the code here we'll take the definition of ``collect`` from Stream as well. It just walks
through the entire Stream and collects the values, ignoring the final unit value.

```haskell
collect :: Monad m => Stream m a () -> m [a]
collect str = go str []
 where
  go str acc = do
    r <- runStream str
    case r of
      Left () -> return (reverse acc)
      Right (a, str') -> go str' (a:acc)
```

Now we can try out ``yield`` using monadic notation:

```haskell
yield123 :: Stream IO Int ()
yield123 = do
  yield 1
  yield 2
  yield 3
```

```
*Main> collect yield123
[1,2,3]
```

We can mix normal Haskell control structures like if/then/else into the monadic notation:

```haskell
yieldEvens :: Int -> Stream IO Int ()
yieldEvens n = if n > 10
                  then return ()
                  else do yield n
                          yieldEvens $ n + 2
```

```
*Main> collect $ yieldEvens 0
[0,2,4,6,8,10]
```

We could read some files using our ``readFileS`` function and yield the results:

```haskell
readAFewFiles :: Stream IO B.ByteString ()
readAFewFiles = do
  readFileS "MapM.lhs" >>= yield
  readFileS "MapM.lhs" >>= yield
  readFileS "MapM.lhs" >>= yield
  readFileS "MapM.lhs" >>= yield
  readFileS "MapM.lhs" >>= yield
```

```
*Main> length <$> collect readAFewFiles
5
```

We can generalise this to apply a monadic function to a list of arguments, which is basically what ``mapM`` does:

```haskell
streamMapM :: (String -> IO B.ByteString) -> [String] -> Stream IO B.ByteString ()
streamMapM _ [] = return ()
streamMapM f (a:as) = do
  (liftIO $ f a) >>= yield
  streamMapM f as
```

And we can even make an infinite stream:

```haskell
readForever :: Stream IO B.ByteString ()
readForever = streamMapM B.readFile (repeat "MapM.lhs")
```

Take from a stream and a definition of head for a stream:

```haskell
takeStream :: Integer -> Stream IO a () -> IO [a]
takeStream n str = go str [] n
 where
  go str acc n = do
    if n <= 0 then return acc
              else do r <- runStream str
                      case r of
                          Left ()         -> return (reverse acc)
                          Right (a, str') -> go str' (a:acc) (n - 1)

headStream :: Stream IO a () -> IO (Maybe a)
headStream str = do
  h <- takeStream 1 str
  return $ case h of
              [h'] -> Just h'
              _    -> Nothing
```

So we can efficiently take the head of the stream without evaluating the entire thing:

```
*Main> (fmap B.length) <$> headStream readForever
Just 5917
```

I should point out that the example of reading a file a bunch of times could be achieved without ``Stream`` just by storing a list of the monadic actions, and then evaluating the one that we want:

```haskell
listOfActions :: [IO B.ByteString]
listOfActions = repeat $ B.readFile "MapM.lhs"
```

which can be used as follows:

```
*Main> B.length <$> (head $ listOfActions)
6455
```

The difference is that the list is somewhat static, in that we can't mix control structures into it as we can do with ``Stream``.

Interestingly, the definition for ``Stream`` looks very similar to the definition for ``Free``, which I used in an [earlier post about free monads](/2014/06/07/notes-on-free-monads):

```haskell
data Stream m a = Nil      | Stream a (m (Stream m a))

data Free   f r = MkPure r | MkFree   (f (Free   f r))
```

Here's one way to encode ``Stream``-like behaviour using free monads. I define two actions, yield and final. The yield action stores an input value of type ``a``, a monadic function ``a -> IO b``, and the rest of the structure, which turns out to be conveniently represented as a function ``b -> k``. Being a function of ``b`` lets the rest of the structure depend on the result at the current node in the structure.  The final action just stores the value and monadic action, and is a terminal node in the free monad.

```haskell
data StreamF a b k = Yield a (a -> IO b) (b -> k)
                   | Final a (a -> IO b)
```

For convenience, ``Command`` is a simpler type signature:

```haskell
type Command a b k = Free (StreamF a b) k
```

As in my earlier post, we need instances for ``Functor`` and ``Monad``. They are fairly straightforward:

```haskell
instance Functor (StreamF a b) where
  fmap f (Yield a io k) = Yield a io (f . k)
  fmap _ (Final a io)   = Final a io

instance (Functor f) => Monad (Free f) where
    return :: a -> Free f a
    return x = MkPure x

    (>>=) :: Free f a -> (a -> Free f b) -> Free f b
    (MkFree x) >>= h = MkFree $ fmap (\q -> q >>= h) x
    (MkPure r) >>= f = f r
```

Here are two helpers to make ``Command``'s monadic usage easier:

```haskell
-- Lift an IO action to a final Command.
finalF :: a -> (a -> IO b) -> Command a b r
finalF a io = MkFree $ Final a io

-- Lift an IO action to a Command that yields the value
-- and continues.
yieldF :: a -> (a -> IO b) -> Command a b b
yieldF a io = MkFree $ Yield a io (\b -> MkPure b)
```

To run a ``Command`` we walk its structure recursively and
run the IO actions as needed:

```haskell
runCommand :: (Show a, Show b, Show r) => Command a b r -> IO ()

runCommand (MkFree (Final a io)) = do
  putStrLn $ "Final " ++ show a
  x <- io a
  putStrLn $ "Produced the value: " ++ show x

runCommand (MkFree (Yield a io next)) = do
  b <- io a
  putStrLn $ "Yield: computed value: " ++ show b
  runCommand (next b)

runCommand (MkPure x) = putStrLn $ "MkPure: " ++ show x
```

As with ``Stream``, we can mix control structures with the creation of the free monad:

```haskell
exampleCommand :: Command FilePath String String
exampleCommand = do
  x <- yieldF "hello1.txt" readFile
  y <- if x == "hello1\n"
          then yieldF "hello2.txt" readFile
          else finalF "hello3.txt" readFile
  return y
```

For example:

```
Yield: computed value: "hello1\n"
Yield: computed value: "hello2\n"
MkPure: "hello2\n"
```

Taking the head of a ``Command`` is straightforward using the definition of ``runCommand``:

```haskell
headCommand :: Command a r r -> IO r
headCommand (MkFree (Final a io  )) = io a
headCommand (MkFree (Yield a io _)) = io a
headCommand (MkPure x)              = return x
```

Here it is in action:

```
*Main> :t headCommand exampleCommand
headCommand exampleCommand :: IO String

*Main> headCommand exampleCommand
"hello1\n"
```

To finish things off, here are versions of ``take`` and ``mapM``
on ``Command``:

```haskell
runOneCommand :: Command t t () -> IO (Either () (t, Command t t ()))

runOneCommand (MkFree (Final a io)) = do
  x <- io a
  return $ Right (x, MkPure ())

runOneCommand (MkFree (Yield a io next)) = do
  b <- io a
  return $ Right (b, next b)

runOneCommand (MkPure ()) = Left <$> return ()

takeCommand :: Integer -> Command t t () -> IO [t]
takeCommand n str = go str [] n
 where
  go str acc n = do
    if n <= 0 then return acc
              else do r <- runOneCommand str
                      case r of
                          Left ()         -> return $ reverse acc
                          Right (a, str') -> go str' (a:acc) (n - 1)

commandMapM :: (a -> IO a) -> [a] -> Command a a ()
commandMapM _ [] = MkPure ()
commandMapM f (a:as) = do
  yieldF a f
  commandMapM f as
```

It works like the ``Stream`` example:

```haskell
takeCommandExample = (fmap B.length) <$> (takeCommand 3 $ commandMapM readFileBB (take 100000 $ repeat "MapM.lhs")) >>= print
 where
    -- Since B.readFile :: String -> B.ByteString
    -- we have to write this wrapper so that the input
    -- and result types match, as required by the
    -- restriction "Command t t ()" in the signature
    -- for takeCommand.
    readFileBB :: B.ByteString -> IO B.ByteString
    readFileBB = B.readFile . (map w2c) . B.unpack
```

There we go:

```
*Main> takeCommandExample
[11241,11241,11241]
```
