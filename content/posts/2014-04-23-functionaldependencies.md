---
author: Carlo Hamalainen

date: "2014-04-23T00:00:00Z"
format: image
title: FunctionalDependencies
url: /2014/04/23/functionaldependencies/
---

Suppose we have two datatypes, ``OptBool`` and ``OptFile`` for storing boolean and file path options. Perhaps this might be for a program that provides an interface to legacy command line applications.

```haskell
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}

module Fundep where

data OptBool = OptBool { optBoolDesc   :: String
                       , optBoolValue  :: Bool
                       } deriving Show

data OptFile = OptFile { optFileDesc :: String
                       , optFileValue :: FilePath
                       } deriving Show
```

We'd like to be able to set the value of an option without having to specify the record name, so instead of

```haskell
opt { optBoolValue = True }
```

we want to write

```haskell
setValue opt True
```

As a first attempt we make a type class ``Option``. We have enabled ``MultiParamTypeClasses`` because the type signature for ``setValue`` has to refer to the option, of type ``a``, and the value of type ``b``. We also enable ``TypeSynonymInstances`` and ``FlexibleInstances`` since ``FilePath`` is a type synonym.

```haskell
class Option a b where
    setDesc   :: a -> String -> a
    setValue  :: a -> b -> a
```

Instance declarations:

```haskell
instance Option OptBool Bool where
    setDesc opt d  = opt { optBoolDesc  = d }
    setValue opt b = opt { optBoolValue = b }

instance Option OptFile FilePath where
    setDesc opt d  = opt { optFileDesc  = d }
    setValue opt f = opt { optFileValue = f }
```

All seems well but the following code doesn't compile:

```haskell
opt1' = setDesc (OptBool "bool" True) "boolean option"
```

with the error message

```
    No instance for (Option OptBool b1) arising from a use of `setDesc'
    The type variable `b1' is ambiguous
    Possible fix: add a type signature that fixes these type variable(s)
    Note: there is a potential instance available:
      instance Option OptBool Bool -- Defined at Fundeps.lhs:40:12
    Possible fix: add an instance declaration for (Option OptBool b1)
    In the expression: setDesc (OptBool "bool" True) "boolean option"
    In an equation for opt1':
        opt1' = setDesc (OptBool "bool" True) "boolean option"
```

The problem is that both ``a`` and ``b`` in the class declaration are free variables, but really this is not the case.  The trick is to enable the ``FunctionalDependencies`` language extension, and then specify that the type ``a`` in the class declaration for ``Option`` implies the type ``b``. This makes sense if you think about the type of ``setValue``. Once we know the type of the first parameter, we then know the type of the value field (assuming that the instance declaraion uses ``OptBoolValue`` or ``optFileValue`` or whatever).

```haskell
class Option a b | a -> b where
    setDesc   :: a -> String -> a
    setValue  :: a -> b -> a
```

Now this is ok:

```haskell
opt1' :: OptBool
opt1' = setDesc (OptBool "bool" True) "boolean option"
```

As a final note, writing the implication ``b -> a`` as below

```haskell
class Option a b | b -> a where
    setDesc   :: a -> String -> a
    setValue  :: a -> b -> a
```

restricts us unnecessarily. If we had another type with a boolean value field,

```haskell
data OptBool' = OptBool' { optBoolDesc'  :: String
                         , optBoolValue' :: Bool
                         } deriving Show

instance Option OptBool' Bool where
    setDesc opt d  = opt { optBoolDesc'  = d }
    setValue opt b = opt { optBoolValue' = b }
```

then this code would not compile

```haskell
opt1'' :: OptBool'
opt1'' = setDesc (OptBool' "bool" True) "boolean option"
```

due to

```
    Functional dependencies conflict between instance declarations:
      instance Option OptBool Bool -- Defined at Fundeps.lhs:41:12
      instance Option OptBool' Bool -- Defined at Fundeps.lhs:91:12
```

In contrast the implication ``a -> b`` means that, for example, the type ``OptBool`` implies the type ``Bool``.

Literate Haskell source for this blog post is available here: [https://github.com/carlohamalainen/playground/tree/master/haskell/fundeps](https://github.com/carlohamalainen/playground/tree/master/haskell/fundeps).


