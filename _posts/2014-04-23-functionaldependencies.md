---
id: 806
title: FunctionalDependencies
date: 2014-04-23T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2014/04/23/functionaldependencies/
permalink: /2014/04/23/functionaldependencies/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---

Suppose we have two datatypes, ``OptBool`` and ``OptFile`` for storing boolean and file path options. Perhaps this might be for a program that provides an interface to legacy command line applications.

{% highlight haskell %}
> {-# LANGUAGE MultiParamTypeClasses  #-}
> {-# LANGUAGE TypeSynonymInstances   #-}
> {-# LANGUAGE FlexibleInstances      #-}
> {-# LANGUAGE FunctionalDependencies #-}
>
> module Fundep where

> data OptBool = OptBool { optBoolDesc   :: String
>                        , optBoolValue  :: Bool
>                        } deriving Show

> data OptFile = OptFile { optFileDesc :: String
>                        , optFileValue :: FilePath
>                        } deriving Show
{% endhighlight %}

We'd like to be able to set the value of an option without having to specify the record name, so instead of 

{% highlight haskell %}
opt { optBoolValue = True }
{% endhighlight %}

we want to write 

{% highlight haskell %}
setValue opt True
{% endhighlight %}

As a first attempt we make a type class ``Option``. We have enabled ``MultiParamTypeClasses`` because the type signature for ``setValue`` has to refer to the option, of type ``a``, and the value of type ``b``. We also enable ``TypeSynonymInstances`` and ``FlexibleInstances`` since ``FilePath`` is a type synonym.

{% highlight haskell %}
class Option a b where
    setDesc   :: a -> String -> a
    setValue  :: a -> b -> a
{% endhighlight %}

Instance declarations: 

{% highlight haskell %}
instance Option OptBool Bool where
    setDesc opt d  = opt { optBoolDesc  = d }
    setValue opt b = opt { optBoolValue = b }

instance Option OptFile FilePath where
    setDesc opt d  = opt { optFileDesc  = d }
    setValue opt f = opt { optFileValue = f }
{% endhighlight %}

All seems well but the following code doesn't compile: 

{% highlight haskell %}
opt1' = setDesc (OptBool "bool" True) "boolean option"
{% endhighlight %}

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

{% highlight haskell %}
class Option a b | a -> b where
    setDesc   :: a -> String -> a
    setValue  :: a -> b -> a
{% endhighlight %}

Now this is ok: 

{% highlight haskell %}
opt1' :: OptBool
opt1' = setDesc (OptBool "bool" True) "boolean option"
{% endhighlight %}

As a final note, writing the implication ``b -> a`` as below

{% highlight haskell %}
class Option a b | b -> a where
    setDesc   :: a -> String -> a
    setValue  :: a -> b -> a
{% endhighlight %}

restricts us unnecessarily. If we had another type with a boolean value field, 

{% highlight haskell %}
data OptBool' = OptBool' { optBoolDesc'  :: String
                         , optBoolValue' :: Bool
                         } deriving Show

instance Option OptBool' Bool where
    setDesc opt d  = opt { optBoolDesc'  = d }
    setValue opt b = opt { optBoolValue' = b }
{% endhighlight %}

then this code would not compile 

{% highlight haskell %}
opt1'' :: OptBool'
opt1'' = setDesc (OptBool' "bool" True) "boolean option"
{% endhighlight %}

due to 

```
    Functional dependencies conflict between instance declarations:
      instance Option OptBool Bool -- Defined at Fundeps.lhs:41:12
      instance Option OptBool' Bool -- Defined at Fundeps.lhs:91:12
```

In contrast the implication ``a -> b`` means that, for example, the type ``OptBool`` implies the type ``Bool``. 

Literate Haskell source for this blog post is available here: <a href="https://github.com/carlohamalainen/playground/tree/master/haskell/fundeps">https://github.com/carlohamalainen/playground/tree/master/haskell/fundeps</a>. 


