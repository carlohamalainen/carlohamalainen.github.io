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
**Update 2011-06-29:** The title of this post should really be **&#8220;Think about your grammer&#8217;s level of nondeterminism before writing an inflamatory blog post about monadic parsing.&#8221;** I would like to thank the commenters for bringing various solutions to my attention. Also, someone seems to have [posted it to reddit](http://www.reddit.com/r/haskell/comments/i9nrn/parsing_with_monads_can_be_slow/) and there is a good conversation over there.

This post was written using [BlogLiterately](http://hackage.haskell.org/package/BlogLiterately). The literate Haskell source file is [parse\_speed\_test.lhs](https://s3.amazonaws.com/carlo-hamalainen.net/oldblog/stuff/haskell/parse_speed_test.lhs).

In late 2010 I went to a meeting of the Brisbane Functional Programmers Group. Tony Morris talked about using Haskell for parsing large GPS datasets. If I recall correctly, Tony said something like &#8220;you need to use arrows for parsing large datasets, not monads, because using monads will be slow.&#8221;

Someone from the audience asked why this was the case, but no one was able to give a clear answer. I decided to write a small parser using monads in Haskell and collect some run times. In the code below, I borrowed a few definitions from [some slides of an earlier talk by Tony Morris at BFPG](http://projects.tmorris.net/public/haskell-parsers/artifacts/0.9/html/index.html#MyParser.hs), such as bindParser, mapParser, sequenceParser, etc. Tony&#8217;s slides are quite good so I won&#8217;t attempt to repeat his explanation of how monads can be used for parsing.

<pre>&gt; import Data.Char
&gt; import Maybe
&gt; import System.Environment
&gt;
&gt; data Parser a = P {
&gt;   parse :: String -&gt; Maybe (String, a)
&gt; }
&gt;
&gt; instance Monad Parser where
&gt;   (&gt;&gt;=) = bindParser
&gt;   return = value
&gt;
&gt; failed :: Parser a
&gt; failed = P (_ -&gt; Nothing)
&gt;
&gt; character :: Parser Char
&gt; character = P (s -&gt; case s of [] -&gt; Nothing
&gt;                                (c:r) -&gt; Just (r, c))
&gt;
&gt; (|||) :: Parser a -&gt; Parser a -&gt; Parser a
&gt; P p1 ||| P p2 = P (s -&gt; case p1 s of v@(Just _) -&gt; v
&gt;                                       Nothing -&gt; p2 s)
&gt;
&gt; mapParser :: Parser a -&gt; (a -&gt; b) -&gt; Parser b
&gt; mapParser (P p) f = P (s -&gt; case p s of Just (r, c) -&gt; Just (r, f c)
&gt;                                          Nothing -&gt; Nothing)
&gt;
&gt; bindParser :: Parser a -&gt; (a -&gt; Parser b) -&gt; Parser b
&gt; bindParser (P p) f = P (s -&gt; case p s of Just (r, c) -&gt; parse (f c) r
&gt;                                           Nothing -&gt; Nothing)
&gt;
&gt; value :: a -&gt; Parser a
&gt; value a = P (s -&gt; Just (s, a))
&gt;
&gt; (&gt;&gt;&gt;) :: Parser a -&gt; Parser b -&gt; Parser b
&gt; p &gt;&gt;&gt; q = bindParser p (_ -&gt; q)
&gt;
&gt; sequenceParser :: [Parser a] -&gt; Parser [a]
&gt; sequenceParser [] = value []
&gt; sequenceParser (h:t) = bindParser h (a -&gt; mapParser (sequenceParser t) (as -&gt; a : as))
&gt;
&gt; list :: Parser a -&gt; Parser [a]
&gt; list k = many1 k ||| value []
&gt;
&gt; many1 :: Parser a -&gt; Parser [a]
&gt; many1 k = bindParser k (k' -&gt; mapParser (list k) (kk' -&gt; k' : kk'))
&gt;
&gt; satisfy :: (Char -&gt; Bool) -&gt; Parser Char
&gt; satisfy p = bindParser character (c -&gt; if p c then value c else failed)
&gt;
&gt; is :: Char -&gt; Parser Char
&gt; is c = satisfy (== c)
&gt;
&gt; space :: Parser Char
&gt; space = satisfy isSpace
&gt;
&gt; alpha :: Parser Char
&gt; alpha = satisfy isAlpha
</pre>

Let&#8217;s try to parse an XML-like document. For simplicity we will use angle brackets (as in normal XML) and allow any other character in a tag&#8217;s name:

<pre>&gt; nonAngleBracket c = not $ c `elem` ['']
&gt;
&gt; xmlTagNameParser :: Parser String
&gt; xmlTagNameParser = many1 (satisfy nonAngleBracket)
</pre>

An open tag has a left angle bracket, some characters for the name, and a right angle bracket. This can be written very succintly in Haskell with do notation:

<pre>&gt; openTagParser :: Parser String
&gt; openTagParser = do list space
&gt;                    is '                    tag_name                     is '&gt;'
&gt;                    return (head (words tag_name))
</pre>

Similarly for a closing tag:

<pre>&gt; closeTagParser :: Parser String
&gt; closeTagParser = do list space
&gt;                     is '                     is '/'
&gt;                     tag_name                      is '&gt;'
&gt;                     return tag_name
</pre>

The text inside a basic XML structure can be any ASCII character except for an opening bracket.

<pre>&gt; xmlTextFieldParser :: Parser String
&gt; xmlTextFieldParser = list (satisfy (c -&gt; isAscii c && (c /= '&lt;&#039;)))
</pre>

The simplest XML structure that we will parse is a balanced pair of tags with some text inbetween, such as: hey there.

<pre>&gt; xmlTextElementParser :: Parser [(String, String)]
&gt; xmlTextElementParser = do open_tag_name                              text_body                                  close_tag_name                             if open_tag_name /= close_tag_name then failed else return [(close_tag_name, text_body)]
</pre>

The other kind of structure that we can parse is a pair of balanced tags with some list of text bits inside, such as:

bar valuemeh value

The top level definition of the parse is thus:

<pre>&gt; xmlToTagValueList :: Parser [(String, String)]
&gt; xmlToTagValueList =
&gt;   do open_tag_name          body                   close_tag_name         if open_tag_name /= close_tag_name then failed else return ((close_tag_name, "-&gt;"):(concat body))
</pre>

For testing purposes we will try to parse the input file and then report the number of parsed XML-like elements.

<pre>&gt; main = do args            x            let x' = parse (list xmlToTagValueList) x
&gt;           print (length (head (snd (fromJust x'))))
</pre>

For testing I will use a file with one line repeated inside a block. The file input10.txt has 10 lines in the inner block:

<pre>do de dah  bar 
      do de dah  bar 
      do de dah  bar 
      do de dah  bar 
      do de dah  bar 
      do de dah  bar 
      do de dah  bar 
      do de dah  bar 
      do de dah  bar 
      do de dah  bar 
 
 </pre>

Here are some run times on a lightly loaded 2Ghz Xeon running Ubuntu 10.10 with GHC 6.12.1, and the binary compiled with &#8220;ghc -O &#8211;make&#8221;. This parse is clearly impractical if parsing 30 lines of text will take over 1.5 hours:

<table border="1">
  <tr>
    <th>
      Nr lines
    </th>
    
    <th>
      time (seconds)
    </th>
  </tr>
  
  <tr>
    <td>
      1
    </td>
    
    <td>
      0.116
    </td>
  </tr>
  
  <tr>
    <td>
      10
    </td>
    
    <td>
      0.015
    </td>
  </tr>
  
  <tr>
    <td>
      15
    </td>
    
    <td>
      0.468
    </td>
  </tr>
  
  <tr>
    <td>
      20
    </td>
    
    <td>
      16.166
    </td>
  </tr>
  
  <tr>
    <td>
      25
    </td>
    
    <td>
      466.365
    </td>
  </tr>
  
  <tr>
    <td>
      30
    </td>
    
    <td>
      > 5000
    </td>
  </tr>
</table>

This kind of issue turns out to be a well known problem with monadic parsing. The answer comes from John Hugh&#8217;s 1998 paper [Generalising Monads to Arrows](http://www.ittc-ku.net/Projects/SLDG/filing_cabinet/Hughes_Generalizing_Monads_to_Arrows.pdf), from which I now quote:

> Although the idea of programming with combinators is quite old, the design of combinator libraries has been profoundly infuenced in recent years by Wadler&#8217;s introduction of the concept of a monad into functional programming[Wad90, Wad92, Wad95]. We shall discuss monads much more fully in the next section, but for now, suffice it to say that a monad is a kind of standardised interface to an abstract data type of \`program fragments&#8217;. The monad interface has been found to be suitable for many combinator libraries, and is now extensively used. Numerous benefits flow from using a common interface: to take just one example, Haskell has been extended with special constructions to make the use of monads particularly convenient.
> 
> It is therefore a matter for some concern when libraries emerge which cannot, for fundamental reasons, use the monad interface. In particular, Swierstra and Duponcheel have developed [a very interesting library for parsing LL-1 grammars[SD96]](http://people.cs.uu.nl/doaitse/Papers/1996/LL1.pdf), that avoids a well-known inefficiency in monadic parsing libraries by combining the construction of a parser with a \`static analysis&#8217; of the program so constructed. Yet Swierstra and Duponcheel&#8217;s optimisation is incompatible with the monad interface. We believe that their library is not just an isolated example, but demonstrates a generally useful paradigm for combinator design that falls outside the world of monads. We shall look more closely at their idea in section 3. Inspired by Swierstra and Duponcheel&#8217;s library, I sought a generalisation of the monad concept that could also offer a standardised interface to libraries ofthis new type. My proposal, which I call _arrows_, is the subject of this paper. Pleasingly, the arrow interface turned out to be applicable to other kinds of non-monadic library also, for example the _fudgets_ library for graphical user interfaces [CH93], and a new library for programming active web pages. These applications will be described in sections 6 and 9.
> 
> While arrows are a little less convenient to use than monads, they have significantly wider applicability. They can therefore be used to bring the benefits of monad-like programming to a much wider class of applications.

The paper by Swierstra and Duponcheel pretty much sums things up:

> As soon as one starts to use normal combinator based parsers, disappointments arise. The parsers constructed may be unexpectedly slow, and since combinators are usually used to describe non-deterministic parsers, they do not perform any form of error-reporting, let alone error-recovery. Even the smallest mistake in the input may lead to a lengthy parsing process which finally produces an empty list of successful parses, with no clue as to where where the mistake is located.

To learn about arrows in Haskell, go here: <http://www.haskell.org/arrows/>.

**Archived Comments**

Date: 2011-06-27 06:12:37 UTC

Author: Heinrich Apfelmus

Errm, while there are some optimizations that can only be done with parser combinators based on Arrows or Applicative, it appears to me that your example is simply a bad case of excessive backtracking. 🙂 Parser combinators are not entirely foolproof, you always have to put a little effort in combinator design and grammar design to get usable performance (aka not exponential).

Date: 2011-06-27 07:38:00 UTC

Author: Vincent Toups

&#8220;As soon as one starts to use normal combinator based parsers, disappointments arise. The parsers constructed may be unexpectedly slow, and since combinators are usually used to describe non-deterministic parsers, they do not perform any form of error-reporting, let alone error-recovery. Even the smallest mistake in the input may lead to a lengthy parsing process which finally produces an empty list of successful parses, with no clue as to where where the mistake is located.&#8221;

As I understand it, you just need to use the right monad. Your parser monad can be combined with an error monad or a continuation monad to provide more sophisticated error handling features. The last slide of my recent talk about this subject mentions this, briefly: <a href="http://dorophone.blogspot.com/2011/05/monadic-parser-combinators-in-elisp.html" rel="nofollow">http://dorophone.blogspot.com/2011/05/monadic-parser-combinators-in-elisp.html</a>

Date: 2011-06-28 00:48:18 UTC

Author: Malcolm Wallace

The problems with error reporting and performance mentioned in a 15-year-old paper have been addressed long ago, by many researchers. For one treatment of exactly these problems (errors and performance), which even uses your example of simplified XML-parsing, see <a href="http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.135.7512&#038;rep=rep1&#038;type=pdf" rel="nofollow">http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.135.7512&rep=rep1&type=pdf</a>  
Don&#8217;t miss the performance tables which (for instance) demonstrate parsing of a 1 million element XML file.

Date: 2011-06-29 15:02:43 UTC

Author: Brandon

Monadic parsers may miss out on some optimizations, but the main problem here is the massive nondeterminism in your grammar. You allow &#8216;/&#8217; in tag names, so can be interpreted as an opening tag as well.

Change

> nonAngleBracket c = not $ c \`elem\` [&#8221;]

to

> nonAngleBracket c = not $ c \`elem\` [&#8221;, &#8216;/&#8217;]

and a test file with 20,000 lines parses in under a second.

Do you have any tests showing that other styles of parser combinators (or parser generators) can handle handle the original ambiguous grammar any better?

Date: 2011-06-29 21:09:00 UTC

Author: carlo

Brandon: Firstly, WordPress messed up the &#8221; in your comment so I edited it on your behalf &#8211; I hope this is ok. I tried your suggestion and indeed it is blazingly fast.

Secondly, no, I have not tried any other styles of parser combinators.