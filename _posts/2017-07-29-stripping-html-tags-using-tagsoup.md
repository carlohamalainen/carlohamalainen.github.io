---
id: 839
title: Stripping html tags using TagSoup
date: 2017-07-29T09:32:47+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/?p=839
permalink: /2017/07/29/stripping-html-tags-using-tagsoup/
jabber_published:
  - "1501320768"
categories:
  - Uncategorized
---
I had a situation, when converting old blog posts to WordPress, where I wanted to strip all the extra info on the `pre` tags. For example this:

<pre class="brush: plain; title: ; notranslate" title="">&lt;pre&gt;&lt;code&gt;&lt;span style=&quot;&quot;&gt;&amp;gt;&lt;/span&gt; &lt;span style=&quot;color: blue; font-weight: bold;&quot;&gt;import&lt;/span&gt; &lt;span style=&quot;&quot;&gt;Data&lt;/span&gt;&lt;span style=&quot;&quot;&gt;.&lt;/span&gt;&lt;span style=&quot;&quot;&gt;Char&lt;/span&gt;
</pre>

would turn into: 

<pre class="brush: plain; title: ; notranslate" title="">&gt;import Data.Char
</pre>

It turns out that this is really easy using [TagSoup](https://hackage.haskell.org/package/tagsoup).

<pre class="brush: plain; title: ; notranslate" title="">module Detag where

import Control.Monad
import Text.HTML.TagSoup
</pre>

The function to strip tags works on a list of tags of strings: 

<pre class="brush: plain; title: ; notranslate" title="">strip :: [Tag String] -&gt; [Tag String]

strip [] = []
</pre>

If we hit a `pre` tag, ignore its info (the underscore) and continue on recursively: 

<pre class="brush: plain; title: ; notranslate" title="">strip (TagOpen &quot;pre&quot; _ : rest) = TagOpen &quot;pre&quot; [] : strip rest
</pre>

Similarly, strip the info off an opening code tag: 

<pre class="brush: plain; title: ; notranslate" title="">strip (TagOpen  &quot;code&quot; _ : rest) = strip rest
strip (TagClose &quot;code&quot;   : rest) = strip rest
</pre>

If we hit a span, followed by some text, and a closing span, then keep the text tag and continue: 

<pre class="brush: plain; title: ; notranslate" title="">strip (TagOpen &quot;span&quot; _ : TagText t : TagClose &quot;span&quot; : rest)
  = TagText t : strip rest
</pre>

Don&#8217;t change other tags: 

<pre class="brush: plain; title: ; notranslate" title="">strip (t:ts) = t : strip ts
</pre>

Parsing input from stdin is straightforward. We use `optEscape` and `optRawTag` to avoid mangling other html in the input.

<pre class="brush: plain; title: ; notranslate" title="">main :: IO ()
main = do
    s &lt;- getContents
    let tags = parseTags s
        ropts = renderOptions{optEscape = id, optRawTag = const True}
    putStrLn $ renderTagsOptions ropts $ strip tags
</pre>

Example output: 

<pre class="brush: plain; title: ; notranslate" title="">$ runhaskell Detag.hs 
&lt;pre class=&quot;sourceCode haskell&quot;&gt;&lt;code class=&quot;sourceCode haskell&quot;&gt;&lt;span style=&quot;&quot;&gt;&amp;gt;&lt;/span&gt; &lt;span style=&quot;color: green;&quot;&gt;{-# LANGUAGE RankNTypes          #-}&lt;/span&gt;
&lt;pre&gt;&gt; {-# LANGUAGE RankNTypes          #-}
</pre>