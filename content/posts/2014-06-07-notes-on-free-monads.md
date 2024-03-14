---
author: Carlo Hamalainen

date: "2014-06-07T00:00:00Z"
format: image
guid: http://carlo-hamalainen.net/2014/06/07/notes-on-free-monads/
id: 688
original_post_id:
- "16"
restapi_import_id:
- 596a05ef0330b
title: Notes on free monads
url: /2014/06/07/notes-on-free-monads/
---


These are my personal notes on David Laing's talk [Free monads are good for you - BFPG - 2014-04-22](https://vimeo.com/92992647). His code and slides are available here: [https://github.com/dalaing/bfpg-2014-04](https://github.com/dalaing/bfpg-2014-04). My code diverges from his in that I don't use ``liftF`` and instead write my own definition of ``Free``, and my own monad instance, which follows the style in Gabriel Gonzalez's blog post [Why free monads matter](http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html). For me this made it easier to see the monad structure, mainly because I wanted to avoid the details of ``wrap`` and ``liftF`` in [Control.Monad.Free.Class](http://hackage.haskell.org/package/free-4.7.1/docs/Control-Monad-Free-Class.html).

Literate haskell source is here: [https://github.com/carlohamalainen/playground/tree/master/haskell/notes-on-free-monads-are-good-for-you](https://github.com/carlohamalainen/playground/tree/master/haskell/notes-on-free-monads-are-good-for-you)

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}

module Notes where

import qualified Data.Text as T
import qualified Data.Map as M
import Control.Monad (forM_, mapM)
import Safe (lastDef)
import Data.Maybe (fromJust)

-- My usual friends.
data Hole = Hole
hole :: forall a. a
hole = undefined
```

Motivating example: model a data store. This could be an in-memory dictionary, an sqlite file on disk, a postgresql table, etc.

Parameterize the type by the type of the index, ``i``, and the type of the values, ``v``.

```haskell
data DataStore i v = Create   (v  -> i)
                   | List     (() -> [v])
                   | Retrieve (i  -> v)
                   | Update v
                   | Delete i
```

How to read the types: for ``Create``, ``List``, and ``Retrieve``, an action is performed. So it's natural that these three constructors will store a function type.

* ``Create``: we supply a value ``v``, and get back an index ``i``.
* ``List``:   we supply nothing (I guess in a more general setting we could supply a filter?) and get back a list of values.
* ``Retrieve``: We supply an index and get back a value.

On the other hand, ``Update``and ``Delete`` are "in-place." In theory we could return a list of affected indexes, so ``Update`` could have been ``Update (v -> [i])``. But we'll leave it as just ``Update v``. Lastly, ``Delete`` just deletes the index/value pair, so we don't have anything to return, so it's not a function type.

Later we want to make our data store an instance of ``Free`` so we need data store to be an instance of ``Functor``. Check the kind of ``Free`` in ghci:

```
ghci> :k Free
Free :: (* -> *) -> * -> *
```

However our ``DataStore`` has a concrete type. So add another parameter ``k`` and tweak the types:

```haskell
data DataStoreF i v k = Create v   (i   -> k)
                      | List       ([v] -> k)
                      | Retrieve i (v   -> k)
                      | Update v k
                      | Delete i k
```

This is a bit like a continuation. Reading the types:

* ``Create``: the value ``v`` is part of the constructor, and the last bit is a function that uses the new index ``i`` and produces some ``k``.
* ``List``:   now the annoying ``()``  has  gone, and the constructor just holds the function which takes the list ``[v]`` and does something with it, returning something of type ``k``.
* ``Retrieve``: the index is stored along with the function that uses the retrieved value, returning something of type ``k``.
* ``Update``: As before, but since nothing is done "to" the updated value, we can just store the value of type ``k``.
* ``Delete``: similar to Update.

For the Functor instance we can use the compiler extension ``DerivingInstances`` or we can grind through the details ourselves. It's easy using [hole-driven development](http://matthew.brecknell.net/post/hole-driven-haskell/) because there are so few choices for each definition.

```haskell
instance Functor (DataStoreF i v) where
    fmap f (Create v k)   = Create v   (f . k)
    fmap f (List k)       = List       (f . k)
    fmap f (Retrieve i k) = Retrieve i (f . k)
    fmap f (Update v k)   = Update v (f k)
    fmap f (Delete i k)   = Delete i (f k)
```

We can sequence commands manually. For example, create 3, then delete that same value, and finally return ``()``.

```haskell
manual1 :: DataStoreF i Integer (DataStoreF i v ())
manual1 = Create 3 (\i -> (Delete i ()))
```

Create 3, then create 4, then create 5, and return the triple
consisting of the index values for 3, 4, and 5, respectively:

```haskell
manual2 :: DataStoreF t Integer (DataStoreF t Integer (DataStoreF t Integer (t, t, t)))
manual2 = Create 3 (\i ->
          Create 4 (\j ->
          Create 5 (\k ->
          (i, j, k))))
```

Even though ``manual1`` and ``manual2`` are sequences of commands, their type changes (and gets longer as more commands are added). But we can use ``Fix`` (or ``Free``), which can be thought of as the [fixed point of a functor](http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html).

```haskell
data Free f r = MkFree (f (Free f r)) | MkPure r
```

Now a data store with index ``i``, key ``v``, and return type ``r``:

```haskell
type DataStore i v r = Free (DataStoreF i v) r
```

Now ``manual1``and ``manual2`` have a succint type:

```haskell
manual1' :: DataStore i Integer ()
manual1' = MkFree $ Create 3 (\i ->
                    (MkFree $ Delete i (MkPure ())))

manual2' :: Free (DataStoreF i Integer) (i, i, i)
manual2' = MkFree $ Create 3 (\i ->
                    MkFree $ Create 4 (\j ->
                    MkFree $ Create 5 (\k ->
                    MkPure (i, j, k))))
```

This looks suspiciously like the monad bind syntax, desugared. In fact, for any functor ``f``, we can write the instance for ``Monad (Free f)``:

```haskell
instance (Functor f) => Monad (Free f) where
    return :: a -> Free f a
    return x = MkPure x

    (>>=) :: Free f a -> (a -> Free f b) -> Free f b
    (MkFree x) >>= h = MkFree $ fmap (\q -> q >>= h) x
    (MkPure r) >>= f = f r
```

The function return puts a value into the monad, which uses ``MkPure``. The bind definition is a bit more complicated, but it can be worked out using hole driven development:

The two parameters:

```haskell
x :: f (Free f a)
h :: a -> Free f b
```

Use the ``MkFree`` constructor, poke the hole:

```haskell
    (MkFree x) >>= h = MkFree Hole
```

which results in

```
Couldn't match expected type `f (Free f b)' with actual type `Hole'
```

We have ``h`` which produces a ``Free f b`` from an ``a`` but ``x`` has type ``f (...)``. So maybe ``fmap`` will help:

```haskell
    (MkFree x) >>= h = MkFree $ fmap Hole x
```

Now it says:

```
Couldn't match expected type `Free f a -> Free f b'
```

Make it a function:

```haskell
    (MkFree x) >>= h = MkFree $ fmap (\q -> Hole) x
```

Which now complains:

```
Couldn't match expected type `Free f b' with actual type `Hole'
```

Temporarily turn ``Hole`` into ``hole`` and use ghc-mod to check the type of ``q``. We now have these terms:

```haskell
q :: Free f a
h :: a -> Free f b
```

But look at the type of ``>>=``, it is exactly what we need now:

```haskell
(>>=) :: Free f a -> (a -> Free f b) -> Free f b
```

So plug it in and it type checks. (We have to manually verify the monad laws ourselves.)

```haskell
    (MkFree x) >>= h = MkFree $ fmap (\q -> q >>= h) x
```

This can also be written as

```haskell
    (MkFree x) >>= h = MkFree $ fmap (>>= h) x
```

Now we can use ``do`` notation to create a sequence of commands:

```haskell
manual1'' :: DataStore i Integer ()
manual1'' = do
  i <- MkFree $ Create 3 (\x0 -> MkPure x0)
  MkFree $ Delete i (MkPure ())
  return ()

manual2'' :: DataStore i Integer (i, i, i)
manual2'' = do
  i <- MkFree $ Create 3 (\x0 -> MkPure x0)
  j <- MkFree $ Create 4 (\x1 -> MkPure x1)
  k <- MkFree $ Create 5 (\x2 -> MkPure x2)
  return (i, j, k)
```

Some helper functions:

```haskell
create :: v -> DataStore i v i
create x = MkFree $ Create x (\i -> MkPure i)

delete :: i -> DataStore i v ()
delete i = MkFree $ Delete i (MkPure ())

manual1''' :: DataStore i Integer ()
manual1''' = do
  i <- create 3
  delete i
  return ()

manual2''' :: DataStore i Integer (i, i, i)
manual2''' = do
  i <- create 3
  j <- create 4
  k <- create 5
  return (i, j, k)
```

So we have a nifty little DSL!

We can write an "interpreter" that converts a series of data store commands into a string:

```haskell
printDataStore :: (Show i, Show r) => DataStore i i r -> String

printDataStore (MkFree (Create v next)) = "Create: " ++ show v ++ "\n"
                                          ++ printDataStore (next v)
```

Notice how the ``next`` is a function, so to print it we first apply it to the index value that we have.

```haskell
v    :: i
next :: i -> Free (DataStoreF i i) r
```

With ``List`` we run into a problem. We aren't actually computing the data store, we have no way of producing an actual list to print here. So we just use the empty list as a dummy list for the parameter to 'next'.

```haskell
printDataStore (MkFree (List next)) = "List (dummy list): [pretend list] " ++ "\n"
                                      ++ printDataStore (next dummyList)
  where dummyList = [] :: [i]
```

The rest of these are straightforward:

```haskell
printDataStore (MkFree (Retrieve i next)) = "Retrieve: " ++ show i ++ "\n"
                                            ++ printDataStore (next i)

printDataStore (MkFree (Delete i next)) = "Delete: " ++ show i ++ "\n"
                                          ++ printDataStore next

printDataStore (MkFree (Update i next)) = "Update: " ++ show i ++ "\n"
                                          ++ printDataStore next

printDataStore (MkPure x) = "Pure value: " ++ show x
```

Some examples:

```
ghci> putStrLn $ printDataStore manual1'''
Create: 3
Delete: 3
Pure value: ()

ghci> putStrLn $ printDataStore manual2'''
Create: 3
Create: 4
Create: 5
Pure value: (3,4,5)
```

Make a few more helpers so we can do a longer example:

```haskell
list :: DataStore i v [v]
list = MkFree $ List (\x -> MkPure x)

retrieve :: i -> DataStore i v v
retrieve i = MkFree $ Retrieve i (\x -> MkPure x)

update :: v -> DataStore i v ()
update v = MkFree $ Update v (MkPure ())

longExample :: DataStore Integer Integer (Integer, [Integer])
longExample = do
  i <- create 3
  j <- create 4
  values <- list
  v <- retrieve i
  return (v, values)
```

So while the value of the index for ``3`` passes through as ``i``, the list stored in ``values`` is just the empty list as can be seen in the final value:

```
ghci> putStrLn $ printDataStore longExample
Create: 3
Create: 4
List (dummy list): [pretend list]
Retrieve: 3
Pure value: (3,[])
```

It's a bit like having a void back end that ignores all commands to store data and always returns an empty list.

Since this DSL is built on top of the monadic do-notation, we can use normal Haskell language features, for example if/then/else statements:

```haskell
egLogic :: DataStore Integer Integer (Either Integer [Integer])
egLogic = do
  i <- create 3
  j <- create 4
  values <- list
  if length values `mod` 2 == 0
      then return $ Right values
      else do v <- retrieve i
              return $ Left v
```

But we don't see the if/then/else when we use ``printDataStore``, as it's embedded in the monadic functions:

```
ghci> putStrLn $ printDataStore egLogic
Create: 3
Create: 4
List (dummy list): [pretend list]
Pure value: Right []
```

We ended up going down the ``then`` path of the ``if/then/else`` because our ``printDataStore`` function uses the empty list when expanding the ``List`` case.

Finally, here is a way to interpret the free monad and actually compute the data store, using an in-memory ``Map``:

```haskell
runInMap :: (Ord i, Show i, Show r) => M.Map i i -> DataStore i i r -> (r, M.Map i i)
runInMap store (MkFree (Create v next))   = runInMap (M.insert v v store) (next v)
runInMap store (MkFree (List next))       = runInMap store (next (M.elems store))
runInMap store (MkFree (Retrieve i next)) = runInMap store (next (fromJust $ M.lookup i store))
runInMap store (MkFree (Delete i next))   = runInMap (M.delete i store) next
runInMap _ (MkFree (Update _ _))          = error "Update is not implemented" -- TODO
runInMap store (MkPure x) = (x, store)
```

```
ghci> runInMap  M.empty egLogic
(Right [3,4],fromList [(3,3),(4,4)])

ghci> runInMap (M.fromList [(10,10)]) egLogic
(Left 3,fromList [(3,3),(4,4),(10,10)])
```

Another example, this time using ``mapM`` in the midst of the DSL:

```haskell
egLogic' :: Free (DataStoreF Integer Integer) [Integer]
egLogic' = do
  keys <- mapM create [10..20]
  values <- mapM retrieve keys
  return values
```

```
ghci> runInMap  M.empty egLogic'
([10,11,12,13,14,15,16,17,18,19,20],
fromList [(10,10),(11,11),(12,12),(13,13),(14,14),(15,15),(16,16),(17,17),(18,18),(19,19),(20,20)])
```
