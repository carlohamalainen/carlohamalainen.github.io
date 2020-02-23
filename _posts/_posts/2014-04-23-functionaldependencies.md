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
Suppose we have two datatypes, OptBool and OptFile for storing boolean and file path options. Perhaps this might be for a program that provides an interface to legacy command line applications.

<pre>&gt; {-# LANGUAGE MultiParamTypeClasses  #-}
&gt; {-# LANGUAGE TypeSynonymInstances   #-}
&gt; {-# LANGUAGE FlexibleInstances      #-}
&gt; {-# LANGUAGE FunctionalDependencies #-}
&gt;
&gt; module Fundep where
</pre>

<pre>&gt; data OptBool = OptBool { optBoolDesc   :: String
&gt;                        , optBoolValue  :: Bool
&gt;                        } deriving Show
</pre>

<pre>&gt; data OptFile = OptFile { optFileDesc :: String
&gt;                        , optFileValue :: FilePath
&gt;                        } deriving Show
</pre>

We’d like to be able to set the value of an option without having to specify the record name, so instead of 

<pre>&gt; opt { optBoolValue = True }
</pre>

we want to write 

<pre>&gt; setValue opt True
</pre>

As a first attempt we make a type class Option:, where we have enabled MultiParamTypeClasses because the type signature for setValue has to refer to the option, of type a, and the value of type b. We also enable TypeSynonymInstances and FlexibleInstances since FilePath is a type synonym. 

<pre>&gt; class Option a b where
&gt;     setDesc   :: a -&gt; String -&gt; a
&gt;     setValue  :: a -&gt; b -&gt; a
</pre>

Instance declarations: 

<pre>&gt; instance Option OptBool Bool where
&gt;     setDesc opt d  = opt { optBoolDesc  = d }
&gt;     setValue opt b = opt { optBoolValue = b }
&gt;
&gt; instance Option OptFile FilePath where
&gt;     setDesc opt d  = opt { optFileDesc  = d }
&gt;     setValue opt f = opt { optFileValue = f }
</pre>

All seems well but the following code doesn’t compile: 

<pre>&gt; opt1' = setDesc (OptBool "bool" True) "boolean option"
</pre>

with the error message 

<pre>No instance for (Option OptBool b1) arising from a use of `setDesc'
    The type variable `b1' is ambiguous
    Possible fix: add a type signature that fixes these type variable(s)
    Note: there is a potential instance available:
      instance Option OptBool Bool -- Defined at Fundeps.lhs:40:12
    Possible fix: add an instance declaration for (Option OptBool b1)
    In the expression: setDesc (OptBool "bool" True) "boolean option"
    In an equation for opt1':
        opt1' = setDesc (OptBool "bool" True) "boolean option"
</pre>

The problem is that both a and b in the class declaration are free variables, but really this is not the case. The trick is to enable the FunctionalDependencies language extension, and then specify that the type a in the class declaration for Option implies the type b. This makes sense if you think about the type of setValue. Once we know the type of the first parameter, we then know the type of the value field (assuming that the instance declaraion uses OptBoolValue or optFileValue or whatever).

<pre>&gt; class Option a b | a -&gt; b where
&gt;     setDesc   :: a -&gt; String -&gt; a
&gt;     setValue  :: a -&gt; b -&gt; a
</pre>

Now this is ok: 

<pre>&gt; opt1' :: OptBool
&gt; opt1' = setDesc (OptBool "bool" True) "boolean option"
</pre>

As a final note, writing the implication b -> a as below

<pre>&gt; class Option a b | b -&gt; a where
&gt;     setDesc   :: a -&gt; String -&gt; a
&gt;     setValue  :: a -&gt; b -&gt; a
</pre>

restricts us unnecessarily. If we had another type with a boolean value field, 

<pre>&gt; data OptBool' = OptBool' { optBoolDesc'  :: String
&gt;                          , optBoolValue' :: Bool
&gt;                          } deriving Show
</pre>

<pre>&gt; instance Option OptBool' Bool where
&gt;     setDesc opt d  = opt { optBoolDesc'  = d }
&gt;     setValue opt b = opt { optBoolValue' = b }
</pre>

then this code would not compile 

<pre>&gt; opt1'' :: OptBool'
&gt; opt1'' = setDesc (OptBool' "bool" True) "boolean option"
</pre>

due to 

<pre>Functional dependencies conflict between instance declarations:
      instance Option OptBool Bool -- Defined at Fundeps.lhs:41:12
      instance Option OptBool' Bool -- Defined at Fundeps.lhs:91:12
</pre>

In contrast the implication a -> b means that, for example, the type OptBool implies the type Bool. 

Literate Haskell source for this blog post is available here: <https://github.com/carlohamalainen/playground/tree/master/haskell/fundeps>.