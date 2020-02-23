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

<pre>&gt; {-# LANGUAGE OverloadedStrings, InstanceSigs #-}
&gt;
&gt; import Control.Applicative
&gt; import Control.Monad
&gt; import qualified Data.ByteString as B
&gt; import Data.ByteString.Internal (w2c)
&gt; import Data.Either
</pre>

I recently wrote some code using [wreq](http://hackage.haskell.org/package/wreq) that seemed to use much more memory than I thought it should. The problem turned out not to be with wreq but with the way that I was using mapM. An equivalent snippet of code is: 

<pre>&gt; main1 = do
&gt;   firstFile &lt;- head  mapM B.readFile (take 100000 $ repeat "MapM.lhs")
&gt;   print $ B.length firstFile
</pre>

I reasoned that mapM would construct its result lazily, then head would force evaluation of just the first element of the list. This isn’t the case, as [explained here](http://stackoverflow.com/questions/3270255/is-haskells-mapm-not-lazy). The function mapM is basically equivalent to this: 

<pre>&gt; mapM' :: Monad m =&gt; (a -&gt; m b) -&gt; [a] -&gt; m [b]
&gt; mapM' m [] = return []
&gt; mapM' m (x:xs) = do
&gt;   x'    xs'    return (x':xs')
</pre>

So the monadic action m is evaluated to build up the list elements. 

One of the answers on the StackOverflow page says to use a step by step series to only evaluate the bits that are required:

<pre>&gt; data Stream m a = Nil | Stream a (m (Stream m a))
</pre>

GHC 7.8.3 comes with [Stream](http://www.haskell.org/ghc/docs/latest/html/libraries/ghc/src/Stream.html) defined as: 

<pre>&gt; -- In GHC 7.8.3:
&gt; newtype Stream m a b = Stream { runStream :: m (Either b (a, Stream m a b)) }
</pre>

The idea is that it represents a sequence of monadic actions. A Left is a final value of type b, while Right (a, Stream m a b) represents an intermediate value of type a along with the remaining stream. 

The Monad instance is fairly straightforward. The return function turns a plain value into a final value (hence the Left), and the bind either stops with the final value or produces the new value along with the next stream. 

<pre>&gt; instance Monad m =&gt; Monad (Stream m a) where
&gt;   return a = Stream $ return $ Left a
&gt;   Stream m &gt;&gt;= k = Stream $ do
&gt;                       r                        case r of
&gt;                           Left b         -&gt; runStream $ k b
&gt;                           Right (a, str) -&gt; return $ Right (a, str &gt;&gt;= k)
</pre>

There are also instances for Functor and Applicative but we don’t need them here. 

A handy function is liftIO which turns a normal monadic action into a stream: 

<pre>&gt; liftIO :: IO a -&gt; Stream IO b a
&gt; liftIO io = Stream $ io &gt;&gt;= return . Left
</pre>

It just runs the io action, and pipes it to a Left and then returns it in a Stream.

<pre>&gt; readFileS :: FilePath -&gt; Stream IO b B.ByteString
&gt; readFileS f = liftIO $ B.readFile f
</pre>

To use readFileS we wrap it with runStream: 

<pre>*Main&gt; Left x  print $ B.length x
4243
</pre>

So we can produce final values, but what about intermediate ones? This is what yield does: 

<pre>&gt; yield :: Monad m =&gt; a -&gt; Stream m a ()
&gt; yield a = Stream $ return $ Right $ (a, return ())
</pre>

At this point we have no idea about the remaining stream, so we return the unit (). 

For testing the code here we’ll take the definition of collect from Stream as well. It just walks through the entire Stream and collects the values, ignoring the final unit value.

<pre>&gt; collect :: Monad m =&gt; Stream m a () -&gt; m [a]
&gt; collect str = go str []
&gt;  where
&gt;   go str acc = do
&gt;     r      case r of
&gt;       Left () -&gt; return (reverse acc)
&gt;       Right (a, str') -&gt; go str' (a:acc)
</pre>

Now we can try out yield using monadic notation: 

<pre>&gt; yield123 :: Stream IO Int ()
&gt; yield123 = do
&gt;   yield 1
&gt;   yield 2
&gt;   yield 3
</pre>

<pre>*Main&gt; collect yield123
[1,2,3]
</pre>

We can mix normal Haskell control structures like if/then/else into the monadic notation: 

<pre>&gt; yieldEvens :: Int -&gt; Stream IO Int ()
&gt; yieldEvens n = if n &gt; 10
&gt;                   then return ()
&gt;                   else do yield n
&gt;                           yieldEvens $ n + 2
</pre>

<pre>*Main&gt; collect $ yieldEvens 0
[0,2,4,6,8,10]
</pre>

We could read some files using our readFileS function and yield the results: 

<pre>&gt; readAFewFiles :: Stream IO B.ByteString ()
&gt; readAFewFiles = do
&gt;   readFileS "MapM.lhs" &gt;&gt;= yield
&gt;   readFileS "MapM.lhs" &gt;&gt;= yield
&gt;   readFileS "MapM.lhs" &gt;&gt;= yield
&gt;   readFileS "MapM.lhs" &gt;&gt;= yield
&gt;   readFileS "MapM.lhs" &gt;&gt;= yield
</pre>

<pre>*Main&gt; length  collect readAFewFiles
5
</pre>

We can generalise this to apply a monadic function to a list of arguments, which is basically what mapM does:

<pre>&gt; streamMapM :: (String -&gt; IO B.ByteString) -&gt; [String] -&gt; Stream IO B.ByteString ()
&gt; streamMapM _ [] = return ()
&gt; streamMapM f (a:as) = do
&gt;   (liftIO $ f a) &gt;&gt;= yield
&gt;   streamMapM f as
</pre>

And we can even make an infinite stream: 

<pre>&gt; readForever :: Stream IO B.ByteString ()
&gt; readForever = streamMapM B.readFile (repeat "MapM.lhs")
</pre>

Take from a stream and a definition of head for a stream: 

<pre>&gt; takeStream :: Integer -&gt; Stream IO a () -&gt; IO [a]
&gt; takeStream n str = go str [] n
&gt;  where
&gt;   go str acc n = do
&gt;     if n                else do r                        case r of
&gt;                           Left ()         -&gt; return (reverse acc)
&gt;                           Right (a, str') -&gt; go str' (a:acc) (n - 1)
&gt;
&gt; headStream :: Stream IO a () -&gt; IO (Maybe a)
&gt; headStream str = do
&gt;   h    return $ case h of
&gt;               [h'] -&gt; Just h'
&gt;               _    -&gt; Nothing
</pre>

So we can efficiently take the head of the stream without evaluating the entire thing: 

<pre>*Main&gt; (fmap B.length)  headStream readForever
Just 5917
</pre>

I should point out that the example of reading a file a bunch of times could be achieved without Stream just by storing a list of the monadic actions, and then evaluating the one that we want: 

<pre>&gt; listOfActions :: [IO B.ByteString]
&gt; listOfActions = repeat $ B.readFile "MapM.lhs"
</pre>

which can be used as follows: 

<pre>*Main&gt; B.length  (head $ listOfActions)
6455
</pre>

The difference is that the list is somewhat static, in that we can’t mix control structures into it as we can do with Stream. 

Interestingly, the definition for Stream looks very similar to the definition for Free, which I used in an [earlier post about free monads](/blog/2014/6/7/notes-on-free-monads):

<pre>&gt; data Stream m a = Nil      | Stream a (m (Stream m a))
</pre>

<pre>&gt; data Free   f r = MkPure r | MkFree   (f (Free   f r))
</pre>

Here’s one way to encode Stream-like behaviour using free monads. I define two actions, yield and final. The yield action stores an input value of type a, a monadic function a -> IO b, and the rest of the structure, which turns out to be conveniently represented as a function b -> k. Being a function of b lets the rest of the structure depend on the result at the current node in the structure. The final action just stores the value and monadic action, and is a terminal node in the free monad.

<pre>&gt; data StreamF a b k = Yield a (a -&gt; IO b) (b -&gt; k)
&gt;                    | Final a (a -&gt; IO b)
</pre>

For convenience, Command is a simpler type signature: 

<pre>&gt; type Command a b k = Free (StreamF a b) k
</pre>

As in my earlier post, we need instances for Functor and Monad. They are fairly straightforward: 

<pre>&gt; instance Functor (StreamF a b) where
&gt;   fmap f (Yield a io k) = Yield a io (f . k)
&gt;   fmap _ (Final a io)   = Final a io
&gt;
&gt; instance (Functor f) =&gt; Monad (Free f) where
&gt;     return :: a -&gt; Free f a
&gt;     return x = MkPure x
&gt;
&gt;     (&gt;&gt;=) :: Free f a -&gt; (a -&gt; Free f b) -&gt; Free f b
&gt;     (MkFree x) &gt;&gt;= h = MkFree $ fmap (q -&gt; q &gt;&gt;= h) x
&gt;     (MkPure r) &gt;&gt;= f = f r
</pre>

Here are two helpers to make Command’s monadic usage easier: 

<pre>&gt; -- Lift an IO action to a final Command.
&gt; finalF :: a -&gt; (a -&gt; IO b) -&gt; Command a b r
&gt; finalF a io = MkFree $ Final a io
&gt;
&gt; -- Lift an IO action to a Command that yields the value
&gt; -- and continues.
&gt; yieldF :: a -&gt; (a -&gt; IO b) -&gt; Command a b b
&gt; yieldF a io = MkFree $ Yield a io (b -&gt; MkPure b)
</pre>

To run a Command we walk its structure recursively and run the IO actions as needed: 

<pre>&gt; runCommand :: (Show a, Show b, Show r) =&gt; Command a b r -&gt; IO ()
&gt;
&gt; runCommand (MkFree (Final a io)) = do
&gt;   putStrLn $ "Final " ++ show a
&gt;   x    putStrLn $ "Produced the value: " ++ show x
&gt;
&gt; runCommand (MkFree (Yield a io next)) = do
&gt;   b    putStrLn $ "Yield: computed value: " ++ show b
&gt;   runCommand (next b)
&gt;
&gt; runCommand (MkPure x) = putStrLn $ "MkPure: " ++ show x
</pre>

As with Stream, we can mix control structures with the creation of the free monad: 

<pre>&gt; exampleCommand :: Command FilePath String String
&gt; exampleCommand = do
&gt;   x    y            then yieldF "hello2.txt" readFile
&gt;           else finalF "hello3.txt" readFile
&gt;   return y
</pre>

For example: 

<pre>Yield: computed value: "hello1n"
Yield: computed value: "hello2n"
MkPure: "hello2n"
</pre>

Taking the head of a Command is straightforward using the definition of runCommand: 

<pre>&gt; headCommand :: Command a r r -&gt; IO r
&gt; headCommand (MkFree (Final a io  )) = io a
&gt; headCommand (MkFree (Yield a io _)) = io a
&gt; headCommand (MkPure x)              = return x
</pre>

Here it is in action: 

<pre>*Main&gt; :t headCommand exampleCommand
headCommand exampleCommand :: IO String

*Main&gt; headCommand exampleCommand
"hello1n"
</pre>

To finish things off, here are versions of take and mapM on Command: 

<pre>&gt; runOneCommand :: Command t t () -&gt; IO (Either () (t, Command t t ()))
&gt;
&gt; runOneCommand (MkFree (Final a io)) = do
&gt;   x    return $ Right (x, MkPure ())
&gt;
&gt; runOneCommand (MkFree (Yield a io next)) = do
&gt;   b    return $ Right (b, next b)
&gt;
&gt; runOneCommand (MkPure ()) = Left  return ()
&gt;
&gt; takeCommand :: Integer -&gt; Command t t () -&gt; IO [t]
&gt; takeCommand n str = go str [] n
&gt;  where
&gt;   go str acc n = do
&gt;     if n                else do r                        case r of
&gt;                           Left ()         -&gt; return $ reverse acc
&gt;                           Right (a, str') -&gt; go str' (a:acc) (n - 1)
&gt;
&gt; commandMapM :: (a -&gt; IO a) -&gt; [a] -&gt; Command a a ()
&gt; commandMapM _ [] = MkPure ()
&gt; commandMapM f (a:as) = do
&gt;   yieldF a f
&gt;   commandMapM f as
</pre>

It works like the Stream example: 

<pre>&gt; takeCommandExample = (fmap B.length)  (takeCommand 3 $ commandMapM readFileBB (take 100000 $ repeat "MapM.lhs")) &gt;&gt;= print
&gt;  where
&gt;     -- Since B.readFile :: String -&gt; B.ByteString
&gt;     -- we have to write this wrapper so that the input
&gt;     -- and result types match, as required by the
&gt;     -- restriction "Command t t ()" in the signature
&gt;     -- for takeCommand.
&gt;     readFileBB :: B.ByteString -&gt; IO B.ByteString
&gt;     readFileBB = B.readFile . (map w2c) . B.unpack
</pre>

There we go: 

<pre>*Main&gt; takeCommandExample
[11241,11241,11241]
</pre>