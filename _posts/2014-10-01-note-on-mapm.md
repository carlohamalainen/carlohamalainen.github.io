---
id: 756
title: Note on mapM
date: 2014-10-01T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2014/10/01/note-on-mapm/
permalink: /2014/10/01/note-on-mapm/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
Not to self about mapM. Is it lazy? Sort of. 

Literate source is here: <https://github.com/carlohamalainen/playground/tree/master/haskell/mapm>. 

First, some imports: 

<pre>> {-# LANGUAGE OverloadedStrings, InstanceSigs #-}
>
> import Control.Applicative
> import Control.Monad
> import qualified Data.ByteString as B
> import Data.ByteString.Internal (w2c)
> import Data.Either
</pre>

I recently wrote some code using [wreq](http://hackage.haskell.org/package/wreq) that seemed to use much more memory than I thought it should. The problem turned out not to be with wreq but with the way that I was using mapM. An equivalent snippet of code is: 

<pre>> main1 = do
>   firstFile <- head  mapM B.readFile (take 100000 $ repeat "MapM.lhs")
>   print $ B.length firstFile
</pre>

I reasoned that mapM would construct its result lazily, then head would force evaluation of just the first element of the list. This isn’t the case, as [explained here](http://stackoverflow.com/questions/3270255/is-haskells-mapm-not-lazy). The function mapM is basically equivalent to this: 

<pre>> mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
> mapM' m [] = return []
> mapM' m (x:xs) = do
>   x'    xs'    return (x':xs')
</pre>

So the monadic action m is evaluated to build up the list elements. 

One of the answers on the StackOverflow page says to use a step by step series to only evaluate the bits that are required:

<pre>> data Stream m a = Nil | Stream a (m (Stream m a))
</pre>

GHC 7.8.3 comes with [Stream](http://www.haskell.org/ghc/docs/latest/html/libraries/ghc/src/Stream.html) defined as: 

<pre>> -- In GHC 7.8.3:
> newtype Stream m a b = Stream { runStream :: m (Either b (a, Stream m a b)) }
</pre>

The idea is that it represents a sequence of monadic actions. A Left is a final value of type b, while Right (a, Stream m a b) represents an intermediate value of type a along with the remaining stream. 

The Monad instance is fairly straightforward. The return function turns a plain value into a final value (hence the Left), and the bind either stops with the final value or produces the new value along with the next stream. 

<pre>> instance Monad m => Monad (Stream m a) where
>   return a = Stream $ return $ Left a
>   Stream m >>= k = Stream $ do
>                       r                        case r of
>                           Left b         -> runStream $ k b
>                           Right (a, str) -> return $ Right (a, str >>= k)
</pre>

There are also instances for Functor and Applicative but we don’t need them here. 

A handy function is liftIO which turns a normal monadic action into a stream: 

<pre>> liftIO :: IO a -> Stream IO b a
> liftIO io = Stream $ io >>= return . Left
</pre>

It just runs the io action, and pipes it to a Left and then returns it in a Stream.

<pre>> readFileS :: FilePath -> Stream IO b B.ByteString
> readFileS f = liftIO $ B.readFile f
</pre>

To use readFileS we wrap it with runStream: 

<pre>*Main> Left x  print $ B.length x
4243
</pre>

So we can produce final values, but what about intermediate ones? This is what yield does: 

<pre>> yield :: Monad m => a -> Stream m a ()
> yield a = Stream $ return $ Right $ (a, return ())
</pre>

At this point we have no idea about the remaining stream, so we return the unit (). 

For testing the code here we’ll take the definition of collect from Stream as well. It just walks through the entire Stream and collects the values, ignoring the final unit value.

<pre>> collect :: Monad m => Stream m a () -> m [a]
> collect str = go str []
>  where
>   go str acc = do
>     r      case r of
>       Left () -> return (reverse acc)
>       Right (a, str') -> go str' (a:acc)
</pre>

Now we can try out yield using monadic notation: 

<pre>> yield123 :: Stream IO Int ()
> yield123 = do
>   yield 1
>   yield 2
>   yield 3
</pre>

<pre>*Main> collect yield123
[1,2,3]
</pre>

We can mix normal Haskell control structures like if/then/else into the monadic notation: 

<pre>> yieldEvens :: Int -> Stream IO Int ()
> yieldEvens n = if n > 10
>                   then return ()
>                   else do yield n
>                           yieldEvens $ n + 2
</pre>

<pre>*Main> collect $ yieldEvens 0
[0,2,4,6,8,10]
</pre>

We could read some files using our readFileS function and yield the results: 

<pre>> readAFewFiles :: Stream IO B.ByteString ()
> readAFewFiles = do
>   readFileS "MapM.lhs" >>= yield
>   readFileS "MapM.lhs" >>= yield
>   readFileS "MapM.lhs" >>= yield
>   readFileS "MapM.lhs" >>= yield
>   readFileS "MapM.lhs" >>= yield
</pre>

<pre>*Main> length  collect readAFewFiles
5
</pre>

We can generalise this to apply a monadic function to a list of arguments, which is basically what mapM does:

<pre>> streamMapM :: (String -> IO B.ByteString) -> [String] -> Stream IO B.ByteString ()
> streamMapM _ [] = return ()
> streamMapM f (a:as) = do
>   (liftIO $ f a) >>= yield
>   streamMapM f as
</pre>

And we can even make an infinite stream: 

<pre>> readForever :: Stream IO B.ByteString ()
> readForever = streamMapM B.readFile (repeat "MapM.lhs")
</pre>

Take from a stream and a definition of head for a stream: 

<pre>> takeStream :: Integer -> Stream IO a () -> IO [a]
> takeStream n str = go str [] n
>  where
>   go str acc n = do
>     if n                else do r                        case r of
>                           Left ()         -> return (reverse acc)
>                           Right (a, str') -> go str' (a:acc) (n - 1)
>
> headStream :: Stream IO a () -> IO (Maybe a)
> headStream str = do
>   h    return $ case h of
>               [h'] -> Just h'
>               _    -> Nothing
</pre>

So we can efficiently take the head of the stream without evaluating the entire thing: 

<pre>*Main> (fmap B.length)  headStream readForever
Just 5917
</pre>

I should point out that the example of reading a file a bunch of times could be achieved without Stream just by storing a list of the monadic actions, and then evaluating the one that we want: 

<pre>> listOfActions :: [IO B.ByteString]
> listOfActions = repeat $ B.readFile "MapM.lhs"
</pre>

which can be used as follows: 

<pre>*Main> B.length  (head $ listOfActions)
6455
</pre>

The difference is that the list is somewhat static, in that we can’t mix control structures into it as we can do with Stream. 

Interestingly, the definition for Stream looks very similar to the definition for Free, which I used in an [earlier post about free monads](/blog/2014/6/7/notes-on-free-monads):

<pre>> data Stream m a = Nil      | Stream a (m (Stream m a))
</pre>

<pre>> data Free   f r = MkPure r | MkFree   (f (Free   f r))
</pre>

Here’s one way to encode Stream-like behaviour using free monads. I define two actions, yield and final. The yield action stores an input value of type a, a monadic function a -> IO b, and the rest of the structure, which turns out to be conveniently represented as a function b -> k. Being a function of b lets the rest of the structure depend on the result at the current node in the structure. The final action just stores the value and monadic action, and is a terminal node in the free monad.

<pre>> data StreamF a b k = Yield a (a -> IO b) (b -> k)
>                    | Final a (a -> IO b)
</pre>

For convenience, Command is a simpler type signature: 

<pre>> type Command a b k = Free (StreamF a b) k
</pre>

As in my earlier post, we need instances for Functor and Monad. They are fairly straightforward: 

<pre>> instance Functor (StreamF a b) where
>   fmap f (Yield a io k) = Yield a io (f . k)
>   fmap _ (Final a io)   = Final a io
>
> instance (Functor f) => Monad (Free f) where
>     return :: a -> Free f a
>     return x = MkPure x
>
>     (>>=) :: Free f a -> (a -> Free f b) -> Free f b
>     (MkFree x) >>= h = MkFree $ fmap (q -> q >>= h) x
>     (MkPure r) >>= f = f r
</pre>

Here are two helpers to make Command’s monadic usage easier: 

<pre>> -- Lift an IO action to a final Command.
> finalF :: a -> (a -> IO b) -> Command a b r
> finalF a io = MkFree $ Final a io
>
> -- Lift an IO action to a Command that yields the value
> -- and continues.
> yieldF :: a -> (a -> IO b) -> Command a b b
> yieldF a io = MkFree $ Yield a io (b -> MkPure b)
</pre>

To run a Command we walk its structure recursively and run the IO actions as needed: 

<pre>> runCommand :: (Show a, Show b, Show r) => Command a b r -> IO ()
>
> runCommand (MkFree (Final a io)) = do
>   putStrLn $ "Final " ++ show a
>   x    putStrLn $ "Produced the value: " ++ show x
>
> runCommand (MkFree (Yield a io next)) = do
>   b    putStrLn $ "Yield: computed value: " ++ show b
>   runCommand (next b)
>
> runCommand (MkPure x) = putStrLn $ "MkPure: " ++ show x
</pre>

As with Stream, we can mix control structures with the creation of the free monad: 

<pre>> exampleCommand :: Command FilePath String String
> exampleCommand = do
>   x    y            then yieldF "hello2.txt" readFile
>           else finalF "hello3.txt" readFile
>   return y
</pre>

For example: 

<pre>Yield: computed value: "hello1n"
Yield: computed value: "hello2n"
MkPure: "hello2n"
</pre>

Taking the head of a Command is straightforward using the definition of runCommand: 

<pre>> headCommand :: Command a r r -> IO r
> headCommand (MkFree (Final a io  )) = io a
> headCommand (MkFree (Yield a io _)) = io a
> headCommand (MkPure x)              = return x
</pre>

Here it is in action: 

<pre>*Main> :t headCommand exampleCommand
headCommand exampleCommand :: IO String

*Main> headCommand exampleCommand
"hello1n"
</pre>

To finish things off, here are versions of take and mapM on Command: 

<pre>> runOneCommand :: Command t t () -> IO (Either () (t, Command t t ()))
>
> runOneCommand (MkFree (Final a io)) = do
>   x    return $ Right (x, MkPure ())
>
> runOneCommand (MkFree (Yield a io next)) = do
>   b    return $ Right (b, next b)
>
> runOneCommand (MkPure ()) = Left  return ()
>
> takeCommand :: Integer -> Command t t () -> IO [t]
> takeCommand n str = go str [] n
>  where
>   go str acc n = do
>     if n                else do r                        case r of
>                           Left ()         -> return $ reverse acc
>                           Right (a, str') -> go str' (a:acc) (n - 1)
>
> commandMapM :: (a -> IO a) -> [a] -> Command a a ()
> commandMapM _ [] = MkPure ()
> commandMapM f (a:as) = do
>   yieldF a f
>   commandMapM f as
</pre>

It works like the Stream example: 

<pre>> takeCommandExample = (fmap B.length)  (takeCommand 3 $ commandMapM readFileBB (take 100000 $ repeat "MapM.lhs")) >>= print
>  where
>     -- Since B.readFile :: String -> B.ByteString
>     -- we have to write this wrapper so that the input
>     -- and result types match, as required by the
>     -- restriction "Command t t ()" in the signature
>     -- for takeCommand.
>     readFileBB :: B.ByteString -> IO B.ByteString
>     readFileBB = B.readFile . (map w2c) . B.unpack
</pre>

There we go: 

<pre>*Main> takeCommandExample
[11241,11241,11241]
</pre>