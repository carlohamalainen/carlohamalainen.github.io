This blog post was written using <a href="http://hackage.haskell.org/package/BlogLiterately">BlogLiterately</a>. The literate 
Haskell source file is <a href="/stuff/haskell/monad_motivation.lhs">monad_motivation.lhs</a>.

 <b>Introduction</b><br>

Most languages have a value like the C language's NULL which can be
used to represent "no value" or "invalid value." Because NULL has type
<code>int</code> in the C language, the following program happens to
be valid:

 <pre>
 #include <stdlib.h>
 
 int main()
 {
     int *x = NULL;
     x++;
 
     return 0;
 }
 </pre>

Obviously it's silly to increment NULL by one, but it's fine in C because
NULL is just an integer. Haskell offers us a type system that lets us
make explicit the idea that we either have a value, or we have no value
at all. The Maybe data type is defined in the standard prelude as:

< data Maybe a = Just a | Nothing

 <b>Monads for sheep</b>

To see how to use the <code>Maybe</code> type, we'll follow an
example from [1]. We have a partial family tree for some sheep, who
are represented as integers. Using the father or mother functions we
can work out a sheep's maternal grandfather, or a sheep's mother's
paternal grandfather, etc. Since we don't have complete information about
every sheep's lineage, we return the type <code>Maybe Sheep</code> instead of <code>Sheep</code>.

> type Sheep = Integer
> 
> father :: Sheep -> Maybe Sheep
> father 0 = Just 1
> father 1 = Just 3
> father 2 = Just 5
> father 5 = Just 7
> father _ = Nothing
> 
> mother :: Sheep -> Maybe Sheep
> mother 0 = Just 2
> mother 1 = Just 4
> mother 5 = Just 6
> mother _ = Nothing
> 
> maternalGrandfather :: Sheep -> Maybe Sheep
> maternalGrandfather s = case (mother s) of
>     Nothing -> Nothing
>     Just m  -> father m
> 
> mothersPaternalGrandfather :: Sheep -> Maybe Sheep
> mothersPaternalGrandfather s = case (mother s) of
>     Nothing -> Nothing
>     Just m  -> case (father m) of
>         Nothing -> Nothing
>         Just gf -> father gf

Handling the <code>Just</code> and <code>Nothing</code>
cases becomes tedious as can be seen in the definition of
<code>mothersPaternalGrandfather</code>. Perhaps an improvement would
be to modify the functions <code>mother</code> and <code>father</code>
so that they accept a <code>Maybe Sheep</code> as input:

> father' :: Maybe Sheep -> Maybe Sheep
> father' (Just 0) = Just 1
> father' (Just 1) = Just 3
> father' (Just 2) = Just 5
> father' (Just 5) = Just 7
> father' _ = Nothing
> 
> mother' :: Maybe Sheep -> Maybe Sheep
> mother' (Just 0) = Just 2
> mother' (Just 1) = Just 4
> mother' (Just 5) = Just 6
> mother' _ = Nothing
> 
> maternalGrandfather' :: Sheep -> Maybe Sheep
> maternalGrandfather' s = father' (mother' (Just s))
> 
> mothersPaternalGrandfather' :: Sheep -> Maybe Sheep
> mothersPaternalGrandfather' s = father' . father' $ mother' (Just s)

This is an improvement since we can
write <code>maternalGrandfather'</code> and
<code>mothersPaternalGrandfather'</code> using a single line for
each. However, this comes at the cost of making <code>father'</code>
and <code>mother'</code> take a <code>Maybe Sheep</code> type as input,
and it requires the redundant <code>f Nothing = Nothing</code> line
in each. Why would we call the <code>father'</code> function if we
didn't have a real sheep identifier as input?

So let's try to keep the original <code>father</code> and
<code>mother</code> definitions, with type signatures

< father :: Sheep -> Maybe Sheep
< mother :: Sheep -> Maybe Sheep

What we want is some equivalent way of combining a sequence of calls to
father or mother, with the result that the Nothing value is propagated
along. We can do this with a *combinator*:

> comb :: Maybe a -> (a -> Maybe b) -> Maybe b
> comb Nothing  _ = Nothing
> comb (Just x) f = f x

If we try to combine <code>Nothing</code> with any function we
get <code>Nothing</code> back.  Otherwise we unwrap the value
from the <code>Maybe</code> type and apply the given function. Now
<code>mothersPaternalGrandfather</code> can be rewritten in a much
nicer form:

> mothersPaternalGrandfather2 :: Sheep -> Maybe Sheep
> mothersPaternalGrandfather2 s = (Just s) `comb` mother `comb` father `comb` father 

 <b>Combinators for lists</b>

The sheep combinator seems like a good idea, so it is natural to ask if we can use it for any
other problems. Perhaps there is an analagous combinator for lists?
To find out, let's make a
Lisp-style list data type (here we follow the example in [2]).
A list is either an item followed by another list, or just an empty
list. For example the list <code>[1, 2, 3]</code> would be written as

< Item 1 (Item 2 (Item 3 End))

Let's define the <code>List</code> data type and a few handy functions:

> -- Lisp-style list constructor
> data List a = Item a (List a) | End
>     deriving (Show)
> 
> -- Send a value into a list
> inject :: a -> List a
> inject a = Item a End
> 
> -- Add two lists together. Example:
> -- listAdd (Item 1 (Item 2 (Item 3 End))) (Item 4 (Item 5 End)) = Item 1 (Item 2 (Item 3 (Item 4 (Item 5 End))))
> listAdd :: List a -> List a -> List a
> listAdd End End = End
> listAdd (Item x xs) y = Item x (listAdd xs y)
> listAdd End (Item y ys) = Item y (listAdd End ys)
> 
> -- Concatenate a list of lists. For example:
> -- listConcat (Item (Item 1 (Item 2 (Item 3 End))) End) = Item 1 (Item 2 (Item 3 End))
> listConcat :: List (List a) -> List a
> listConcat End = End
> listConcat (Item x xs) = listAdd x (listConcat xs)
> 
> -- Apply a function f to each element of a list.
> -- Example: listMap (\x -> x + 1) (Item 1 (Item 2 (Item 3 End))) = Item 2 (Item 3 (Item 4 End))
> listMap :: (a -> b) -> List a -> List b
> listMap f End = End
> listMap f (Item x xs) = Item (f x) (listMap f xs)

The sheep combinator had type signature

< Maybe a -> (a -> Maybe b) -> Maybe b

so let's assume that the list combinator has the same type except that
<code>Maybe</code> is replaced with <code>List</code>:

< List a -> (a -> List b) -> List b

We aren't sure how this combinator should be defined, so let's start by supposing that we have
the inputs <code>f :: a -> List a</code> and <code>x :: List a</code>. It seems sensible to apply
<code>f</code> to each element of <code>x</code>. We can do this with the function <code>listMap</code> that we
defined earlier. The expression <code>listMap f x</code> has type
<code>List (List b)</code>. 
To transform
<code>List (List b)</code>
into
<code>List b</code>
we can use <code>listConcat</code>.
This reasoning, purely about
types, in fact leads us to the correct definition of the list
combinator:

> listComb :: List a -> (a -> List b) -> List b
> listComb End _ = End
> listComb list f = listConcat (listMap f list)

Having defined the list combinator, let's see how it could be used in
the simple situation of filtering out elements of a list. A normal list
filter would be written as follows:

> listFilter :: (a -> Bool) -> List a -> List a
> listFilter _ End = End
> listFilter f (Item x xs) = if f x then Item x (listFilter f xs) else listFilter f xs

We can use <code>listFilter</code> to select only the strictly positive
elements of a list:

<pre>
Prelude> :type (> 0)
(> 0) :: (Num a, Ord a) => a -> Bool
Prelude> (> 0) 1
True
Prelude> (> 0) (-10)
False
*Main> listFilter (> 0) (Item 1 (Item 2 (Item (-1) End )))
Item 1 (Item 2 End)
</pre>

In terms of <code>listComb</code>, the input list would be the list
itself. The function must take each element of the input list to a
new list based on some criteria. So given an element <code>x</code>
and function <code>p</code>, the natural definition would be

< x -> if p x then [x] else []

Using our List data type the definition is just:

> myfilter :: (a -> Bool) -> List a -> List a
> myfilter p list =
>     list `listComb` (\x -> if p x then inject x else End)

Here is an example where we select the strictly positive elements of a
list:

<pre>
*Main> myfilter (> 0) (Item 1 (Item 2 (Item (-1) End )))
Item 1 (Item 2 End)
</pre>

 <b>Monads in Haskell</b>

Haskell provides a way to invoke the combinator for a certain type without having to 
specify its name explicitly each time. What we can do is define
<code>List</code> to be an instance of <code>Monad</code> in Haskell's type
system and then use the <code>do</code> notation to invoke the
combinator. This gives an eerily procedural-looking definition of <code>myfilter</code>:

> instance Monad List where
>     return x = inject x
>     list >>= f = listComb list f
>
> myfilter' :: (a -> Bool) -> List a -> List a
> myfilter' p list = do
>     x <- list
>     if p x then inject x else End

The function <code>myfilter'</code> behaves in exactly the same way as
<code>myfilter</code>:

<pre>
*Main> myfilter' (> 0) (Item 1 (Item 2 (Item (-1) End )))
Item 1 (Item 2 End)
</pre>

[As an aside, I think that the <code>do</code> notation is unfortunate.
The English language word "do" implies an action, possibly followed by another action,
but this is not what happens with the list combinator. 
Even worse, to a programmer used to procedural
languages, the <code><-</code> looks like assignment, but in this
(incorrect) imperative reading of <code>myfilter'</code>, the type of
<code>x</code> will be inconsistent (either 
<code>List a</code> or <code>a</code>). On the other hand, a completely new syntax would probably make
people complain that the concept at hand is too alien to even bother with, in the same way that people still
find Lisp's parentheses to be offputting.]

Simon Dobson wrote a blog post [3] explaining another viewpoint of Monads in Haskell: they 
are like a programmable semicolon. In pseudo-C, the
<code>myfilter'</code> Monadic function might look like this:

<pre>
myfilter'(p list)
{
    x <- list ;
    if p x then inject x else End
}
</pre>

It is as if we we have defined the behaviour of ';' *in this particular
situation* to take a list, and then combine it with the function on
the next line using <code>listComb</code>. This is quite a powerful
construct.


 <b>References</b><br>

[1] <a href="http://www.haskell.org/all_about_monads/html/meet.html">http://www.haskell.org/all_about_monads/html/meet.html</a><br>
[2] <a href="http://ianen.org/articles/monad-is-not-difficult/">http://ianen.org/articles/monad-is-not-difficult/</a><br>
[3] <a href="http://www.simondobson.org/2010/06/monads-language-design-perspective/">http://www.simondobson.org/2010/06/monads-language-design-perspective/</a><br>
[4] <a href="http://www.randomhacks.net/articles/2007/03/12/monads-in-15-minutes">http://www.randomhacks.net/articles/2007/03/12/monads-in-15-minutes</a><br>



