---
author: Carlo Hamalainen

date: "2013-05-16T00:00:00Z"
format: image
guid: http://carlo-hamalainen.net/2013/05/16/fiddling-with-gadts/
title: Fiddling with GADTs
url: /2013/05/16/fiddling-with-gadts/
---

```haskell
-- Easiest way to run this with ghci:
--
-- ghci -XDataKinds -XGADTs -XEmptyDataDecls -XKindSignatures -XScopedTypeVariables -XTemplateHaskell

-- Natural numbers, e.g.
-- 0 == Z
-- 1 == S Z
-- 2 == S (S Z)
-- 3 == S (S (S Z))
-- etc

data Nat = Z | S Nat

-- My GADT with two constructors. One would normally
-- write
--
-- data T n = T0 ... | Tm ...
--
-- but here we want to specify the *types* of the constructors.

data T n where
    T0 :: T Z
    Tm :: T n -> T (S n)

-- Calculate the 'n' component, recursing on the definition of Tm.
depth :: T n -> Int
depth T0 = 0
depth (Tm a) = 1 + (depth a)

-- Takes two parameters of type T n, so they must be
-- of the same depth.
twoOfSameDepth :: T n -> T n -> Int
twoOfSameDepth x _ = depth x

-- Takes two parameters T n and T m, and enforces
-- that S n ~ m, in other words the second parameter
-- is one plus the first parameter.
secondIsPlusOne :: forall n m. (S n ~ m) => T n -> T m -> Bool
secondIsPlusOne _ _ = True
```

Check the depth of the structure:

```
*Main> depth T0
0
*Main> depth (Tm T0)
1
*Main> depth (Tm (Tm (Tm (Tm T0))))
4
```


The function twoOfSameDepth expects both parameters to be of the same depth. If not, you get a type error.

```
*Main> :t twoOfSameDepth T0 T0
twoOfSameDepth T0 T0 :: Int
*Main> :t twoOfSameDepth T0 (Tm T0)

:1:20:
    Couldn't match expected type `'Z' with actual type `S n0'
    Expected type: T 'Z
      Actual type: T (S n0)
    In the return type of a call of `Tm'
    In the second argument of `twoOfSameDepth', namely `(Tm T0)'
```


The function secondIsPlusOne expects the second parameter to be of depth one more than the first parameter:

```
*Main> :t secondIsPlusOne T0 (Tm T0)
secondIsPlusOne T0 (Tm T0) :: Bool
*Main> :t secondIsPlusOne T0 (Tm (Tm T0))

:1:25:
    Couldn't match expected type `'Z' with actual type `S n0'
    Expected type: T 'Z
      Actual type: T (S n0)
    In the return type of a call of `Tm'
    In the first argument of `Tm', namely `(Tm T0)'
*Main>
```


In [Matt's LambdaJam talk/jam](http://matthew.brecknell.net/post/btree-gadt/) you can see how this idea can be used to help with implementing a b-tree data structure.
