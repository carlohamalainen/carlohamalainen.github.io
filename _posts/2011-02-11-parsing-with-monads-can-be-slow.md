---
id: 731
title: Parsing with monads (can be) slow
date: 2011-02-11T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2011/02/11/parsing-with-monads-can-be-slow/
permalink: /2011/02/11/parsing-with-monads-can-be-slow/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
**Update 2011-06-29:** The title of this post should really be **"Think about your grammer's level of nondeterminism before writing an inflamatory blog post about monadic parsing."** I would like to thank the commenters for bringing various solutions to my attention. Also, someone seems to have [posted it to reddit](http://www.reddit.com/r/haskell/comments/i9nrn/parsing_with_monads_can_be_slow/) and there is a good conversation over there.

This post was written using <a href="http://hackage.haskell.org/package/BlogLiterately">BlogLiterately</a>. The literate 
Haskell source file is <a href="/stuff/haskell/parse_speed_test.lhs">parse_speed_test.lhs</a>.

In late 2010 I went to a meeting of the Brisbane Functional Programmers Group. Tony Morris talked about
using Haskell for parsing large GPS datasets. If I recall correctly, Tony said something 
like "you need to use arrows for parsing large datasets, not monads, because using monads will be slow."

Someone from the audience asked why this was the case, but no one was able to give a clear answer. I decided to 
write a small parser using monads in Haskell and collect some run times.
In the code below, I borrowed a few definitions from 
<a href="http://projects.tmorris.net/public/haskell-parsers/artifacts/0.9/html/index.html#MyParser.hs">some slides of an earlier talk by Tony Morris at BFPG</a>,
such as bindParser, mapParser, sequenceParser, etc.  Tony's slides are quite good so I won't attempt to repeat his explanation of
how monads can be used for parsing.

{% highlight haskell %}
import Data.Char
import Maybe
import System.Environment   
      
data Parser a = P {
  parse :: String -> Maybe (String, a)
}

instance Monad Parser where
  (>>=) = bindParser
  return = value

failed :: Parser a
failed = P (\_ -> Nothing)

character :: Parser Char
character = P (\s -> case s of [] -> Nothing
                               (c:r) -> Just (r, c))

(|||) :: Parser a -> Parser a -> Parser a
P p1 ||| P p2 = P (\s -> case p1 s of v@(Just _) -> v
                                      Nothing -> p2 s)

mapParser :: Parser a -> (a -> b) -> Parser b
mapParser (P p) f = P (\s -> case p s of Just (r, c) -> Just (r, f c)
                                         Nothing -> Nothing)

bindParser :: Parser a -> (a -> Parser b) -> Parser b
bindParser (P p) f = P (\s -> case p s of Just (r, c) -> parse (f c) r
                                          Nothing -> Nothing)

value :: a -> Parser a
value a = P (\s -> Just (s, a))

(>>>) :: Parser a -> Parser b -> Parser b
p >>> q = bindParser p (\_ -> q)

sequenceParser :: [Parser a] -> Parser [a]
sequenceParser [] = value []
sequenceParser (h:t) = bindParser h (\a -> mapParser (sequenceParser t) (\as -> a : as))

list :: Parser a -> Parser [a]
list k = many1 k ||| value []

many1 :: Parser a -> Parser [a]
many1 k = bindParser k (\k' -> mapParser (list k) (\kk' -> k' : kk'))

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = bindParser character (\c -> if p c then value c else failed)

is :: Char -> Parser Char
is c = satisfy (== c)

space :: Parser Char
space = satisfy isSpace

alpha :: Parser Char
alpha = satisfy isAlpha
{% endhighlight %}

Let's try to parse an XML-like document. For simplicity we will use
angle brackets (as in normal XML) and allow any other character in a tag's name:

{% highlight haskell %}
nonAngleBracket c = not $ c `elem` ['<', '>']

xmlTagNameParser :: Parser String
xmlTagNameParser = many1 (satisfy nonAngleBracket)
{% endhighlight %}

An open tag has a left angle bracket, some characters for the name, and a right angle bracket. This can
be written very succintly in Haskell with ``do`` notation:

{% highlight haskell %}
openTagParser :: Parser String
openTagParser = do list space
                   is '<'
                   tag_name <- xmlTagNameParser
                   is '>'
                   return (head (words tag_name))
{% endhighlight %}

Similarly for a closing tag:

{% highlight haskell %}
closeTagParser :: Parser String
closeTagParser = do list space
                    is '<'
                    is '/'
                    tag_name <- xmlTagNameParser
                    is '>'
                    return tag_name
{% endhighlight %}

The text inside a basic XML structure can be any ASCII character
except for an opening bracket.

{% highlight haskell %}
xmlTextFieldParser :: Parser String
xmlTextFieldParser = list (satisfy (\c -> isAscii c && (c /= '<')))
{% endhighlight %}

The simplest XML structure that we will parse is a balanced pair of tags
with some text inbetween, such as: ``<foo>hey there</foo>``.

{% highlight haskell %}
xmlTextElementParser :: Parser [(String, String)]
xmlTextElementParser = do open_tag_name   <- openTagParser
                          text_body       <- xmlTextFieldParser
                          close_tag_name  <- closeTagParser
                          if open_tag_name /= close_tag_name then failed else return [(close_tag_name, text_body)]
{% endhighlight %}

The other kind of structure that we can parse is a pair of
balanced tags with some list of text bits inside, such as:

    <foo><bar>bar value</bar><meh>meh value</meh></foo>

The top level definition of the parse is thus:

{% highlight haskell %}
xmlToTagValueList :: Parser [(String, String)]
xmlToTagValueList =
  do open_tag_name    <- openTagParser
     body             <- many1 (xmlTextElementParser ||| xmlToTagValueList)
     close_tag_name   <- closeTagParser
     if open_tag_name /= close_tag_name then failed else return ((close_tag_name, "->"):(concat body))
{% endhighlight %}

For testing purposes we will try to parse the input file and then report the number of
parsed XML-like elements.

{% highlight haskell %}
main = do args <- getArgs
          x <- readFile (head args) :: IO String
          let x' = parse (list xmlToTagValueList) x
          print (length (head (snd (fromJust x'))))
{% endhighlight %}

For testing I will use a file with one line repeated inside a
``<blah>`` block. The file input10.txt has 10 lines in the 
inner block:

    <blah>
         <hello> do de dah</hello> <foo> <bar>bar</bar> </foo>
         <hello> do de dah</hello> <foo> <bar>bar</bar> </foo>
         <hello> do de dah</hello> <foo> <bar>bar</bar> </foo>
         <hello> do de dah</hello> <foo> <bar>bar</bar> </foo>
         <hello> do de dah</hello> <foo> <bar>bar</bar> </foo>
         <hello> do de dah</hello> <foo> <bar>bar</bar> </foo>
         <hello> do de dah</hello> <foo> <bar>bar</bar> </foo>
         <hello> do de dah</hello> <foo> <bar>bar</bar> </foo>
         <hello> do de dah</hello> <foo> <bar>bar</bar> </foo>
         <hello> do de dah</hello> <foo> <bar>bar</bar> </foo>
    </blah>

Here are some run times on a lightly loaded 2Ghz Xeon running Ubuntu 10.10
with GHC 6.12.1, and the binary compiled with "ghc -O --make". This parse is clearly impractical if
parsing 30 lines of text will take over 1.5 hours:

 <center>
 <table border="1">
 <tr> <th>Nr lines</th> <th>time (seconds)</th>
 </tr>
 <tr> <td>1</td>  <td>0.116</td> </tr>
 <tr> <td>10</td> <td>0.015</td> </tr>
 <tr> <td>15</td> <td>0.468</td> </tr>
 <tr> <td>20</td> <td>16.166</td> </tr>
 <tr> <td>25</td> <td>466.365</td> </tr>
 <tr> <td>30</td> <td> > 5000 </td> </tr>
 </table>
 </center>

This kind of issue turns out to be a well known problem with monadic parsing.
The answer comes from John Hugh's 1998 paper <a href="http://www.ittc-ku.net/Projects/SLDG/filing_cabinet/Hughes_Generalizing_Monads_to_Arrows.pdf">Generalising Monads to Arrows</a>, from which I now quote:

 <blockquote>
 <p>Although the idea of programming with combinators is quite old,
 the design of combinator libraries has been profoundly infuenced in
 recent years by Wadler's introduction of the concept of a monad into
 functional programming[Wad90, Wad92, Wad95]. We shall discuss monads
 much more fully in the next section, but for now, suffice it to say that
 a monad is a kind of standardised interface to an abstract data type of
 `program fragments'. The monad interface has been found to be suitable
 for many combinator libraries, and is now extensively used. Numerous
 benefits flow from using a common interface: to take just one example,
 Haskell has been extended with special constructions to make the use
 of monads particularly convenient. </p>

 <p>It is therefore a matter for some concern when libraries emerge
 which cannot, for fundamental reasons, use the monad interface. In
 particular, Swierstra and Duponcheel have developed <a href="http://people.cs.uu.nl/doaitse/Papers/1996/LL1.pdf">a very interesting library for parsing LL-1 grammars[SD96]</a>, that avoids a well-known
 inefficiency in monadic parsing libraries by combining the construction
 of a parser with a `static analysis' of the program so constructed. Yet
 Swierstra and Duponcheel's optimisation is incompatible with the
 monad interface. We believe that their library is not just an
 isolated example, but demonstrates a generally useful paradigm for
 combinator design that falls outside the world of monads. We shall
 look more closely at their idea in section 3. Inspired by Swierstra
 and Duponcheel's library, I sought a generalisation of the monad
 concept that could also offer a standardised interface to libraries
 ofthis new type. My proposal, which I call <i>arrows</i>, is the
 subject of this paper. Pleasingly, the arrow interface turned out to
 be applicable to other kinds of non-monadic library also, for example
 the <i>fudgets</i> library for graphical user interfaces [CH93], and
 a new library for programming active web pages. These applications
 will be described in sections 6 and 9.</p>

 <p>While arrows are a little less convenient to use than monads,
 they have significantly wider applicability. They can therefore be
 used to bring the benefits of monad-like programming to a much wider
 class of applications.</p>
 </blockquote>

The paper by Swierstra and Duponcheel pretty much sums things up:

 <blockquote>
 <p> As soon as one starts to use normal combinator based
 parsers, disappointments arise. The parsers constructed may be
 unexpectedly slow, and since combinators are usually used to
 describe non-deterministic parsers, they do not perform any form
 of error-reporting, let alone error-recovery. Even the smallest
 mistake in the input may lead to a lengthy parsing process which
 finally produces an empty list of successful parses, with no clue
 as to where where the mistake is located.
 </p>
 </blockquote>

To learn about arrows in Haskell, go here: <a href="http://www.haskell.org/arrows/">http://www.haskell.org/arrows/</a>.

**Archived Comments**

Date: 2011-06-27 06:12:37 UTC

Author: Heinrich Apfelmus

Errm, while there are some optimizations that can only be done with parser combinators based on Arrows or Applicative, it appears to me that your example is simply a bad case of excessive backtracking. 🙂 Parser combinators are not entirely foolproof, you always have to put a little effort in combinator design and grammar design to get usable performance (aka not exponential).

Date: 2011-06-27 07:38:00 UTC

Author: Vincent Toups

"As soon as one starts to use normal combinator based parsers, disappointments arise. The parsers constructed may be unexpectedly slow, and since combinators are usually used to describe non-deterministic parsers, they do not perform any form of error-reporting, let alone error-recovery. Even the smallest mistake in the input may lead to a lengthy parsing process which finally produces an empty list of successful parses, with no clue as to where where the mistake is located."

As I understand it, you just need to use the right monad. Your parser monad can be combined with an error monad or a continuation monad to provide more sophisticated error handling features. The last slide of my recent talk about this subject mentions this, briefly: <a href="http://dorophone.blogspot.com/2011/05/monadic-parser-combinators-in-elisp.html" rel="nofollow">http://dorophone.blogspot.com/2011/05/monadic-parser-combinators-in-elisp.html</a>

Date: 2011-06-28 00:48:18 UTC

Author: Malcolm Wallace

The problems with error reporting and performance mentioned in a 15-year-old paper have been addressed long ago, by many researchers. For one treatment of exactly these problems (errors and performance), which even uses your example of simplified XML-parsing, see <a href="http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.135.7512&rep=rep1&type=pdf" rel="nofollow">http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.135.7512&rep=rep1&type=pdf</a>  
Don't miss the performance tables which (for instance) demonstrate parsing of a 1 million element XML file.

Date: 2011-06-29 15:02:43 UTC

Author: Brandon

Monadic parsers may miss out on some optimizations, but the main problem here is the massive nondeterminism in your grammar. You allow '/' in tag names, so can be interpreted as an opening tag as well.

Change

{% highlight haskell %}
nonAngleBracket c = not $ c `elem` ["]
{% endhighlight %}

to

{% highlight haskell %}
nonAngleBracket c = not $ c `elem` [", '/']
{% endhighlight %}

and a test file with 20,000 lines parses in under a second.

Do you have any tests showing that other styles of parser combinators (or parser generators) can handle handle the original ambiguous grammar any better?

Date: 2011-06-29 21:09:00 UTC

Author: carlo

Brandon: Firstly, WordPress messed up the " in your comment so I edited it on your behalf -- I hope this is ok. I tried your suggestion and indeed it is blazingly fast.

Secondly, no, I have not tried any other styles of parser combinators.
