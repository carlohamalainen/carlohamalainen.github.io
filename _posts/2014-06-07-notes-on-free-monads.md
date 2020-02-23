---
id: 688
title: Notes on free monads
date: 2014-06-07T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2014/06/07/notes-on-free-monads/
permalink: /2014/06/07/notes-on-free-monads/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
These are my personal notes on David Laing’s talk [Free monads are good for you &#8211; BFPG &#8211; 2014-04-22](https://vimeo.com/92992647). His code and slides are available here: <https://github.com/dalaing/bfpg-2014-04>. My code diverges from his in that I don’t use liftF and instead write my own definition of Free, and my own monad instance, which follows the style in Gabriel Gonzalez’s blog post [Why free monads matter](http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html). For me this made it easier to see the monad structure, mainly because I wanted to avoid the details of wrap and liftF in [Control.Monad.Free.Class](http://hackage.haskell.org/package/free-4.7.1/docs/Control-Monad-Free-Class.html). 

Literate haskell source is here: <https://github.com/carlohamalainen/playground/tree/master/haskell/notes-on-free-monads-are-good-for-you> 

<pre>&gt; {-# LANGUAGE OverloadedStrings #-}
&gt; {-# LANGUAGE RankNTypes #-}
&gt; {-# LANGUAGE InstanceSigs #-}
&gt;
&gt; module Notes where
&gt;
&gt; import qualified Data.Text as T
&gt; import qualified Data.Map as M
&gt; import Control.Monad (forM_, mapM)
&gt; import Safe (lastDef)
&gt; import Data.Maybe (fromJust)
&gt;
&gt; -- My usual friends.
&gt; data Hole = Hole
&gt; hole :: forall a. a
&gt; hole = undefined
</pre>

Motivating example: model a data store. This could be an in-memory dictionary, an sqlite file on disk, a postgresql table, etc. 

Parameterize the type by the type of the index, i, and the type of the values, v. 

<pre>&gt; data DataStore i v = Create   (v  -&gt; i)
&gt;                    | List     (() -&gt; [v])
&gt;                    | Retrieve (i  -&gt; v)
&gt;                    | Update v
&gt;                    | Delete i
</pre>

How to read the types: for Create, List, and Retrieve, an action is performed. So it’s natural that these three constructors will store a function type.

  * Create: we supply a value v, and get back an index i. 
  * List: we supply nothing (I guess in a more general setting we could supply a filter?) and get back a list of values. 
  * Retrieve: We supply an index and get back a value. 

On the other hand, Updateand Delete are “in-place.” In theory we could return a list of affected indexes, so Update could have been Update (v -> [i]). But we’ll leave it as just Update v. Lastly, Delete just deletes the index/value pair, so we don’t have anything to return, so it’s not a function type.

Later we want to make our data store an instance of Free so we need data store to be an instance of Functor. Check the kind of Free in ghci: 

<pre>ghci&gt; :k Free
Free :: (* -&gt; *) -&gt; * -&gt; *
</pre>

However our DataStore has a concrete type. So add another parameter k and tweak the types: 

<pre>&gt; data DataStoreF i v k = Create v   (i   -&gt; k)
&gt;                       | List       ([v] -&gt; k)
&gt;                       | Retrieve i (v   -&gt; k)
&gt;                       | Update v k
&gt;                       | Delete i k
</pre>

This is a bit like a continuation. Reading the types: 

  * Create: the value v is part of the constructor, and the last bit is a function that uses the new index i and produces some k.
  * List: now the annoying () has gone, and the constructor just holds the function which takes the list [v] and does something with it, returning something of type k. 
  * Retrieve: the index is stored along with the function that uses the retrieved value, returning something of type k. 
  * Update: As before, but since nothing is done “to” the updated value, we can just store the value of type k. 
      * Delete: similar to Update. </ul> 
    For the Functor instance we can use the compiler extension DerivingInstances or we can grind through the details ourselves. It’s easy using [hole-driven development](http://matthew.brecknell.net/post/hole-driven-haskell/) because there are so few choices for each definition.
    
    <pre>&gt; instance Functor (DataStoreF i v) where
&gt;     fmap f (Create v k)   = Create v   (f . k)
&gt;     fmap f (List k)       = List       (f . k)
&gt;     fmap f (Retrieve i k) = Retrieve i (f . k)
&gt;     fmap f (Update v k)   = Update v (f k)
&gt;     fmap f (Delete i k)   = Delete i (f k)
</pre>
    
    We can sequence commands manually. For example, create 3, then delete that same value, and finally return (). 
    
    <pre>&gt; manual1 :: DataStoreF i Integer (DataStoreF i v ())
&gt; manual1 = Create 3 (i -&gt; (Delete i ()))
</pre>
    
    Create 3, then create 4, then create 5, and return the triple consisting of the index values for 3, 4, and 5, respectively: 
    
    <pre>&gt; manual2 :: DataStoreF t Integer (DataStoreF t Integer (DataStoreF t Integer (t, t, t)))
&gt; manual2 = Create 3 (i -&gt;
&gt;           Create 4 (j -&gt;
&gt;           Create 5 (k -&gt;
&gt;           (i, j, k))))
</pre>
    
    Even though manual1 and manual2 are sequences of commands, their type changes (and gets longer as more commands are added). But we can use Fix (or Free), which can be thought of as the [fixed point of a functor](http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html).
    
    <pre>&gt; data Free f r = MkFree (f (Free f r)) | MkPure r
</pre>
    
    Now a data store with index i, key v, and return type r: 
    
    <pre>&gt; type DataStore i v r = Free (DataStoreF i v) r
</pre>
    
    Now manual1and manual2 have a succint type: 
    
    <pre>&gt; manual1' :: DataStore i Integer ()
&gt; manual1' = MkFree $ Create 3 (i -&gt;
&gt;                     (MkFree $ Delete i (MkPure ())))
</pre>
    
    <pre>&gt; manual2' :: Free (DataStoreF i Integer) (i, i, i)
&gt; manual2' = MkFree $ Create 3 (i -&gt;
&gt;                     MkFree $ Create 4 (j -&gt;
&gt;                     MkFree $ Create 5 (k -&gt;
&gt;                     MkPure (i, j, k))))
</pre>
    
    This looks suspiciously like the monad bind syntax, desugared. In fact, for any functor f, we can write the instance for Monad (Free f): 
    
    <pre>&gt; instance (Functor f) =&gt; Monad (Free f) where
&gt;     return :: a -&gt; Free f a
&gt;     return x = MkPure x
&gt;
&gt;     (&gt;&gt;=) :: Free f a -&gt; (a -&gt; Free f b) -&gt; Free f b
&gt;     (MkFree x) &gt;&gt;= h = MkFree $ fmap (q -&gt; q &gt;&gt;= h) x
&gt;     (MkPure r) &gt;&gt;= f = f r
</pre>
    
    The function return puts a value into the monad, which uses MkPure. The bind definition is a bit more complicated, but it can be worked out using hole driven development: 
    
    The two parameters: 
    
    <pre>&gt; x :: f (Free f a)
&gt; h :: a -&gt; Free f b
</pre>
    
    Use the MkFree constructor, poke the hole: 
    
    <pre>&gt;     (MkFree x) &gt;&gt;= h = MkFree Hole
</pre>
    
    which results in 
    
    <pre>Couldn't match expected type `f (Free f b)' with actual type `Hole'
</pre>
    
    We have h which produces a Free f b from an a but x has type f (…). So maybe fmap will help: 
    
    <pre>&gt;     (MkFree x) &gt;&gt;= h = MkFree $ fmap Hole x
</pre>
    
    Now it says: 
    
    <pre>Couldn't match expected type `Free f a -&gt; Free f b'
</pre>
    
    Make it a function: 
    
    <pre>&gt;     (MkFree x) &gt;&gt;= h = MkFree $ fmap (q -&gt; Hole) x
</pre>
    
    Which now complains: 
    
    <pre>Couldn't match expected type `Free f b' with actual type `Hole'
</pre>
    
    Temporarily turn Hole into hole and use ghc-mod to check the type of q. We now have these terms: 
    
    <pre>&gt; q :: Free f a
&gt; h :: a -&gt; Free f b
</pre>
    
    But look at the type of >>=, it is exactly what we need now: 
    
    <pre>&gt; (&gt;&gt;=) :: Free f a -&gt; (a -&gt; Free f b) -&gt; Free f b
</pre>
    
    So plug it in and it type checks. (We have to manually verify the monad laws ourselves.) 
    
    <pre>&gt;     (MkFree x) &gt;&gt;= h = MkFree $ fmap (q -&gt; q &gt;&gt;= h) x
</pre>
    
    This can also be written as 
    
    <pre>&gt;     (MkFree x) &gt;&gt;= h = MkFree $ fmap (&gt;&gt;= h) x
</pre>
    
    Now we can use do notation to create a sequence of commands: 
    
    <pre>&gt; manual1'' :: DataStore i Integer ()
&gt; manual1'' = do
&gt;   i  MkPure x0)
&gt;   MkFree $ Delete i (MkPure ())
&gt;   return ()
</pre>
    
    <pre>&gt; manual2'' :: DataStore i Integer (i, i, i)
&gt; manual2'' = do
&gt;   i  MkPure x0)
&gt;   j  MkPure x1)
&gt;   k  MkPure x2)
&gt;   return (i, j, k)
</pre>
    
    Some helper functions: 
    
    <pre>&gt; create :: v -&gt; DataStore i v i
&gt; create x = MkFree $ Create x (i -&gt; MkPure i)
</pre>
    
    <pre>&gt; delete :: i -&gt; DataStore i v ()
&gt; delete i = MkFree $ Delete i (MkPure ())
</pre>
    
    <pre>&gt; manual1''' :: DataStore i Integer ()
&gt; manual1''' = do
&gt;   i    delete i
&gt;   return ()
</pre>
    
    <pre>&gt; manual2''' :: DataStore i Integer (i, i, i)
&gt; manual2''' = do
&gt;   i    j    k    return (i, j, k)
</pre>
    
    So we have a nifty little DSL! 
    
    We can write an “interpreter” that converts a series of data store commands into a string: 
    
    <pre>&gt; printDataStore :: (Show i, Show r) =&gt; DataStore i i r -&gt; String
</pre>
    
    <pre>&gt; printDataStore (MkFree (Create v next)) = "Create: " ++ show v ++ "n"
&gt;                                           ++ printDataStore (next v)
</pre>
    
    Notice how the next is a function, so to print it we first apply it to the index value that we have. 
    
    <pre>&gt; v    :: i
&gt; next :: i -&gt; Free (DataStoreF i i) r
</pre>
    
    With List we run into a problem. We aren’t actually computing the data store, we have no way of producing an actual list to print here. So we just use the empty list as a dummy list for the parameter to ‘next’.
    
    <pre>&gt; printDataStore (MkFree (List next)) = "List (dummy list): [pretend list] " ++ "n"
&gt;                                       ++ printDataStore (next dummyList)
&gt;   where dummyList = [] :: [i]
</pre>
    
    The rest of these are straightforward: 
    
    <pre>&gt; printDataStore (MkFree (Retrieve i next)) = "Retrieve: " ++ show i ++ "n"
&gt;                                             ++ printDataStore (next i)
</pre>
    
    <pre>&gt; printDataStore (MkFree (Delete i next)) = "Delete: " ++ show i ++ "n"
&gt;                                           ++ printDataStore next
</pre>
    
    <pre>&gt; printDataStore (MkFree (Update i next)) = "Update: " ++ show i ++ "n"
&gt;                                           ++ printDataStore next
</pre>
    
    <pre>&gt; printDataStore (MkPure x) = "Pure value: " ++ show x
</pre>
    
    Some examples: 
    
    <pre>ghci&gt; putStrLn $ printDataStore manual1'''
Create: 3
Delete: 3
Pure value: ()

ghci&gt; putStrLn $ printDataStore manual2'''
Create: 3
Create: 4
Create: 5
Pure value: (3,4,5)
</pre>
    
    Make a few more helpers so we can do a longer example: 
    
    <pre>&gt; list :: DataStore i v [v]
&gt; list = MkFree $ List (x -&gt; MkPure x)
</pre>
    
    <pre>&gt; retrieve :: i -&gt; DataStore i v v
&gt; retrieve i = MkFree $ Retrieve i (x -&gt; MkPure x)
</pre>
    
    <pre>&gt; update :: v -&gt; DataStore i v ()
&gt; update v = MkFree $ Update v (MkPure ())
</pre>
    
    <pre>&gt; longExample :: DataStore Integer Integer (Integer, [Integer])
&gt; longExample = do
&gt;   i    j    values    v    return (v, values)
</pre>
    
    So while the value of the index for 3 passes through as i, the list stored in values is just the empty list as can be seen in the final value: 
    
    <pre>ghci&gt; putStrLn $ printDataStore longExample
Create: 3
Create: 4
List (dummy list): [pretend list]
Retrieve: 3
Pure value: (3,[])
</pre>
    
    It’s a bit like having a void back end that ignores all commands to store data and always returns an empty list. 
    
    Since this DSL is built on top of the monadic do-notation, we can use normal Haskell language features, for example if/then/else statements: 
    
    <pre>&gt; egLogic :: DataStore Integer Integer (Either Integer [Integer])
&gt; egLogic = do
&gt;   i    j    values    if length values `mod` 2 == 0
&gt;       then return $ Right values
&gt;       else do v                return $ Left v
</pre>
    
    But we don’t see the if/then/else when we use printDataStore, as it’s embedded in the monadic functions: 
    
    <pre>ghci&gt; putStrLn $ printDataStore egLogic
Create: 3
Create: 4
List (dummy list): [pretend list]
Pure value: Right []
</pre>
    
    We ended up going down the then path of the if/then/else because our printDataStore function uses the empty list when expanding the List case. 
    
    Finally, here is a way to interpret the free monad and actually compute the data store, using an in-memory Map: 
    
    <pre>&gt; runInMap :: (Ord i, Show i, Show r) =&gt; M.Map i i -&gt; DataStore i i r -&gt; (r, M.Map i i)
&gt; runInMap store (MkFree (Create v next))   = runInMap (M.insert v v store) (next v)
&gt; runInMap store (MkFree (List next))       = runInMap store (next (M.elems store))
&gt; runInMap store (MkFree (Retrieve i next)) = runInMap store (next (fromJust $ M.lookup i store))
&gt; runInMap store (MkFree (Delete i next))   = runInMap (M.delete i store) next
&gt; runInMap _ (MkFree (Update _ _))          = error "Update is not implemented" -- TODO
&gt; runInMap store (MkPure x) = (x, store)
</pre>
    
    <pre>ghci&gt; runInMap  M.empty egLogic
(Right [3,4],fromList [(3,3),(4,4)])

ghci&gt; runInMap (M.fromList [(10,10)]) egLogic
(Left 3,fromList [(3,3),(4,4),(10,10)])
</pre>
    
    Another example, this time using mapM in the midst of the DSL: 
    
    <pre>&gt; egLogic' :: Free (DataStoreF Integer Integer) [Integer]
&gt; egLogic' = do
&gt;   keys    values    return values
</pre>
    
    <pre>ghci&gt; runInMap  M.empty egLogic'
([10,11,12,13,14,15,16,17,18,19,20],
fromList [(10,10),(11,11),(12,12),(13,13),(14,14),(15,15),(16,16),(17,17),(18,18),(19,19),(20,20)])
</pre>