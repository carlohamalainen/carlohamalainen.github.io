---
id: 774
title: Applicatives compose, monads do not
date: 2014-01-02T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2014/01/02/applicatives-compose-monads-do-not/
permalink: /2014/01/02/applicatives-compose-monads-do-not/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
### Introduction  


Make sense of this: “applicatives compose, monads do not.” 

More formally, the statement is: let f and g be [applicative functors](http://www.haskell.org/haskellwiki/Applicative_functor). Then the composition f g is also an applicative functor. On the other hand, there exist [monads](http://www.haskell.org/haskellwiki/Monad) f and g such that f g is not a monad. 

### Composing functors 

First we will show how to compose two functors. To make things concrete, let’s do some examples with two data types that are instances of the Functor class in Haskell: [] (lists), and Maybe. We can check this using ghci: 

<pre>Prelude> :set prompt "ghci> "
ghci> :set +t
ghci> :m +Data.Maybe
ghci> :info Maybe
data Maybe a = Nothing | Just a     -- Defined in `Data.Maybe'
instance Eq a => Eq (Maybe a) -- Defined in `Data.Maybe'
instance Monad Maybe -- Defined in `Data.Maybe'
instance Functor Maybe -- Defined in `Data.Maybe'
instance Ord a => Ord (Maybe a) -- Defined in `Data.Maybe'
instance Read a => Read (Maybe a) -- Defined in `GHC.Read'
instance Show a => Show (Maybe a) -- Defined in `GHC.Show'
ghci> :info []
data [] a = [] | a : [a]    -- Defined in `GHC.Types'
instance Eq a => Eq [a] -- Defined in `GHC.Classes'
instance Monad [] -- Defined in `GHC.Base'
instance Functor [] -- Defined in `GHC.Base'
instance Ord a => Ord [a] -- Defined in `GHC.Classes'
instance Read a => Read [a] -- Defined in `GHC.Read'
instance Show a => Show [a] -- Defined in `GHC.Show'
</pre>

In particular, note these lines: 

<pre>instance Functor Maybe -- Defined in `Data.Maybe'
...
instance Functor [] -- Defined in `GHC.Base'
</pre>

Composing [] with Maybe means that we have a list of Maybe values, for example: 

<pre>ghci> [Just 1, Nothing, Just 42] :: [] (Maybe Int)
[Just 1,Nothing,Just 42]
</pre>

Our goal is to write a Functor instance declaration for the general form of this composition, which means having a data type that represents the composition itself. Often it’s easier to start from a specific example and work up to the general case. So let’s start with a list of Maybe values: 

<pre>> {-# LANGUAGE FlexibleInstances, InstanceSigs #-}
>
> module Compose01 where
>
> import Data.Maybe()
>
> data Compose1 = MkCompose1 ([] (Maybe Int))
</pre>

where I have prefixed the data constructor with “Mk” to disambiguate it from the data _type_ which is Compose (this can be helpful for newcomers to Haskell who are not familiar with the usual practice of giving the data type and data constructors the same name). Now generalise on the inner-most type, Int, by making it a parameter:

<pre>> data Compose2 x = MkCompose2 ([] (Maybe x))
</pre>

Next, generalise on the inner-most data constructor, Just, by making it a parameter: 

<pre>> data Compose3 g x = MkCompose3 ([] (g x))
</pre>

Finally, generalise on the list constructor [], making it a parameter: 

<pre>> data Compose f g x = MkCompose (f (g x))
</pre>

and this is our definition of Compose. It lets us represent any composition of data constructors. We can play around with it in ghci: 

<pre>Compose01> :t MkCompose
MkCompose :: f (g x) -> Compose f g x

Compose01> :t MkCompose [[42]] -- list of list
MkCompose [[42]] -- list of list :: Num x => Compose [] [] x

Compose01> :t MkCompose [Just 3, Just 42, Nothing] -- list of Maybe
MkCompose [Just 3, Just 42, Nothing] :: Num x => Compose [] Maybe x
</pre>

Next, we have to fill in the definition of fmap in an instance declaration for Functor: 

<pre>> instance (Functor f, Functor g) => Functor (Compose f g) where
>     fmap f (Compose x) = ...
</pre>

Again, use a concrete example and ghci to guide us. The inner-most type is a Maybe, and being an instance of Functor means that we can usefmap to apply a function to a “boxed” value: 

<pre>Compose01> fmap (x -> x + 1) (Just 42)
Just 43

Compose01> fmap (x -> x + 1) (Nothing)
Nothing

Compose01> :t fmap (x -> x + 1)
fmap (x -> x + 1) :: (Functor f, Num b) => f b -> f b
</pre>

So this function, fmap (x -> x + 1), can be applied to a list using fmap again: 

<pre>Compose01> fmap (fmap (x -> x + 1)) [Just 3, Just 42, Nothing] :: [] (Maybe Int)
[Just 4,Just 43,Nothing]
</pre>

Generalise this by replacing the function (x -> x + 1) with f and the value [Just 3, Just 42, Nothing] with the value z, and we get what turns out to be the correct definition for the instance declaration: 

<pre>> instance (Functor f, Functor g) => Functor (Compose f g) where
>     fmap f (MkCompose x) = MkCompose (fmap (fmap f) x)
</pre>

An exercise for the reader is to check that with this definition of fmap, the functor laws hold: 

<pre>> fmap id = id
> fmap (p . q) = (fmap p) . (fmap q)
</pre>

Now that Compose is an instance of Functor, we can use a single fmap to apply a function on values that are wrapped up in Compose:

<pre>Compose01> fmap (x -> x + 1) (MkCompose [Just 3, Just 42, Nothing])
MkCompose [Just 4,Just 43,Nothing]
</pre>

### Applicatives compose  


To show that applicatives compose, we need to write the instance declaration for Applicative: 

<pre>> instance (Applicative f, Applicative g) => Applicative (Compose f g) where
>     pure x  = ...
>     f  x = ...
</pre>

This one is a bit more complicated than the Functor instance so I made a short screencast on how to use [hole-driven development](http://matthew.brecknell.net/post/hole-driven-haskell/) to find the answer. With hole-driven development we have a bit of a conversation with the type system and this is easier to show in a narrated screencast compared to a linear written text.

<div class="jetpack-video-wrapper">
  <span class="embed-youtube" style="text-align:center; display: block;"></span>
</div>

(Be sure to watch in 720p fullscreen otherwise the text is illegible.) 

If you don’t want to watch the screencast, just take my word that we can fill in the definition for the Compose instance of Applicative. (Or, sneak a peek at the source code for [Control.Applicative.Compose](http://hackage.haskell.org/package/applicative-extras-0.1.8/docs/Control-Applicative-Compose.html).) Another exercise for the reader: verify that the following [functor laws](http://en.wikibooks.org/wiki/Haskell/Applicative_Functors) hold.

<pre>> pure id  v = v                            -- Identity
> pure (.)  u  v  w = u  (v  w) -- Composition
> pure f  pure x = pure (f x)               -- Homomorphism
> u  pure y = pure ($ y)  u              -- Interchange
> fmap f x = pure f  x                      -- Fmap (on the Functor instance)
</pre>

### Monads do not compose 

To show that "monads do not compose", it is sufficient to find a counterexample, namely two [monads](http://www.haskell.org/haskellwiki/Monad) f and g such that f g is not a monad. In particular, we will show that one of the [monad laws](http://www.haskell.org/haskellwiki/Monad_laws) is violated for _any_ possible instance declaration. 

The following is just an expanded version of [Conor McBride’s answer on stackoverflow](http://stackoverflow.com/a/13209294) so all credit goes to him, and any mistakes here are my responsibility. Conor’s proof is the shortest and easiest to explain counterexample that I could find. 

First, define the terminal monad Thud: 

<pre>> data Thud a = MkThud deriving (Show)
</pre>

Note that it has an unused type parameter. We have to do this so that the [kind](http://www.haskell.org/haskellwiki/Kind) is correct for the Monad instance. The instance declaration for Monad is quite easy because we only have a single way of creating a Thud value: 

<pre>> instance Monad Thud where
>     return _ = MkThud
>     _ >>= _  = MkThud
</pre>

Playing around with ghci, we see that anything turns into a Thud: 

<pre>ghci> return 0 :: Thud Int
MkThud

ghci> (return 0 :: Thud Int) >>= (x -> return (x + 1))
MkThud
</pre>

The other data type is Flip, which wraps a value along with a boolean: 

<pre>> data Flip a = MkFlip Bool a deriving (Show)
</pre>

The Monad instance is of a [writer monad](http://www.haskell.org/haskellwiki/All_About_Monads#The_Writer_monad) with an xor structure: 

<pre>> instance Monad Flip where
>     return :: a -> Flip a
>     return = MkFlip False   -- or, return x = MkFlip False x
>
>     (>>=) :: Flip a -> (a -> Flip b) -> Flip b
>     MkFlip False x >>= f = f x
>     MkFlip True  x >>= f = MkFlip (not b) y
>         where MkFlip b y = f x
</pre>

Informally, return wraps a value along with the False value. The bind (>>=) function will apply the monadic function f if we have a False value, otherwise it will apply f but flip its boolean component for the final result. 

Some example values and computations: 

<pre>ghci> (return "boo" :: Flip String)
MkFlip False "boo"

ghci> (return "boo" :: Flip String) >>= (x -> return $ x ++ " hey!")
MkFlip False "boo hey!"

ghci> (return "boo" :: Flip String) >>= (x -> return $ x ++ " hey!") >>= (x -> return $ x ++ " Huh?")
MkFlip False "boo hey! Huh?"

ghci> (return "boo" :: Flip String) >>= (x -> MkFlip True (x ++ " hey!"))
MkFlip True "boo hey!"

ghci> (return "boo" :: Flip String) >>= (x -> MkFlip True (x ++ " hey!")) >>= (x -> return $ x ++ " What?")
MkFlip True "boo hey! What?"

ghci> (return "boo" :: Flip String) >>= (x -> MkFlip True (x ++ " hey!")) >>= (x -> MkFlip True (x ++ " What?"))
MkFlip False "boo hey! What?"
</pre>

Finally we come to the Monad instance for Compose for the specific case of a Flip of Thud: 

<pre>> instance Monad (Compose Flip Thud) where
>     return x = undefined
>     x >>= f  = undefined
</pre>

Let's start with return. It has to produce something of type Compose Flip Thud a, so we begin with the type constructor: 

<pre>>     return x = MkCompose (MkFlip ??? MkThud)
</pre>

This is all we can do -- we are constrained by the types. Now what can go in the place of the three question marks? Perhaps a function of x, say 

<pre>>     return x = MkCompose (MkFlip (h x) MkThud)
</pre>

where h :: a -> Bool. However, Haskell has the parametricity property. Quoting the [Haskell wiki](http://www.haskell.org/haskellwiki/Polymorphism#Parametric_polymorphism): 

> Since a parametrically polymorphic value does not “know” anything about the unconstrained type variables, it must behave the same regardless of its type. This is a somewhat limiting but extremely useful property known as parametricity.

So parametricity implies that the function h can’t be something like 

<pre>h x = if (x is of type blah)
        then True
        else ...
</pre>

which means that h must be a constant, and therefore return is also a constant. Without loss of generality, suppose that the definition is 

<pre>> instance Monad (Compose Flip Thud) where
>     return :: a -> Compose Flip Thud a
>     return x = MkCompose (MkFlip True MkThud)
</pre>

The left identity monad law says that 

<pre>> return x >>= f = f x
</pre>

for any appropriately typed f and x. Since return is a constant, we have 

<pre>> (MkCompose (MkFlip True MkThud)) >>= f = f x
</pre>

Let f = id, then we have two equations using the two values that exist of type Compose Flip Thud: 

<pre>> (MkCompose (MkFlip True MkThud)) >>= id = id (MkCompose (MkFlip True  MkThud))
> (MkCompose (MkFlip True MkThud)) >>= id = id (MkCompose (MkFlip False MkThud))
</pre>

which implies that 

<pre>> id (MkCompose (MkFlip True MkThud)) = id (MkCompose (MkFlip False MkThud))
</pre>

which is a contradiction. So it is not possible to define return and >>= in a consistent manner for the Compose Flip Thud instance of the Monad typeclass. We conclude that in general it is not true that the composition f g will be a monad for any two monads f and g.

### Further reading 

  * Another Stack Overflow question: <http://stackoverflow.com/questions/13034229/concrete-example-showing-that-monads-are-not-closed-under-composition-with-proo> 
  * Philip Wadler’s papers on parametricity: <http://homepages.inf.ed.ac.uk/wadler/topics/parametricity.html>
  * A paper about conditions under which monads _do_ compose: [Eugenia Cheng: Iterated distributive laws](http://arxiv.org/pdf/0710.1120v1.pdf). See also the first Stack Overflow [link](http://stackoverflow.com/questions/7040844/applicatives-compose-monads-dont) for comments about a swap function for reversing the nesting of the monads.

**Archived Comments**

Date: 2014-12-10 13:50:17.111839 UTC

Author: paluh

Thanks for this detailed insight into Applicative and Monad composition (wonderful analysis of Conor McBride's answer) -- it was really helpful for me!

Date: 2014-12-11 13:59:02.180167 UTC

Author: paluh

I have one question regarding your monad composition counterexample. Does this:

<pre>(MkCompose (MkFlip True MkThud)) >>= id
</pre>

typecheck? Can we use id function which has type a -> a in place of function which should have type Monad m => a -> m a?

Date: 2014-12-11 21:28:56.99053 UTC

Author: Carlo

Here is a stand-alone file with the expression that you asked about: [TypeCheckId.hs](https://github.com/carlohamalainen/playground/blob/master/haskell/applicatives_vs_monads_composition/TypeCheckId.hs).

Note the type signature for >>= in the Monad instance for Compose Flip Thud: 

<pre>(>>=) :: Compose Flip Thud a -> (a -> Compose Flip Thud b) -> Compose Flip Thud b
</pre>

If we let a = Compose Flip Thud b then the type signature is:

<pre>(>>=) :: Compose Flip Thud (Compose Flip Thud b) -> (Compose Flip Thud b -> Compose Flip Thud b) -> Compose Flip Thud b
</pre>

So the expression (MkCompose (MkFlip True MkThud)) >>= id does type check.

It does look odd to me. I think the degeneracy of Thud is the cause of this strange situation: we have data Thud a = MkThud so the parameter a can take _any_ value.