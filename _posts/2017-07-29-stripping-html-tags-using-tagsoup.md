---
title: Stripping html tags using TagSoup
date: 2017-07-29T09:32:47+00:00
author: Carlo Hamalainen
layout: post
permalink: /2017/07/29/stripping-html-tags-using-tagsoup/
---
I had a situation, when converting old blog posts to WordPress, where I wanted to strip all the extra info on the `pre` tags. For example this:

    <pre class="brush: plain; title: ; notranslate" title=""><pre><code><span style="">></span> <span style="color: blue; font-weight: bold;">import</span><span style="">Data</span><span style="">.</span><span style="">Char</span>

would turn into: 

    > import Data.Char

It turns out that this is really easy using [TagSoup](https://hackage.haskell.org/package/tagsoup).

{% highlight haskell %}
module Detag where

import Control.Monad
import Text.HTML.TagSoup
{% endhighlight %}

The function to strip tags works on a list of tags of strings: 

{% highlight haskell %}
strip :: [Tag String] -> [Tag String]
strip [] = []
{% endhighlight %}

If we hit a `pre` tag, ignore its info (the underscore) and continue on recursively: 

{% highlight haskell %}
strip (TagOpen "pre" _ : rest) = TagOpen "pre" [] : strip rest
{% endhighlight %}

Similarly, strip the info off an opening code tag: 

{% highlight haskell %}
strip (TagOpen  "code" _ : rest) = strip rest
strip (TagClose "code"   : rest) = strip rest
{% endhighlight %}

If we hit a span, followed by some text, and a closing span, then keep the text tag and continue: 

{% highlight haskell %}
strip (TagOpen "span" _ : TagText t : TagClose "span" : rest)
  = TagText t : strip rest
{% endhighlight %}

Don't change other tags: 

{% highlight haskell %}
strip (t:ts) = t : strip ts
{% endhighlight %}

Parsing input from stdin is straightforward. We use `optEscape` and `optRawTag` to avoid mangling other html in the input.

{% highlight haskell %}
main :: IO ()
main = do
    s <- getContents
    let tags = parseTags s
        ropts = renderOptions{optEscape = id, optRawTag = const True}
    putStrLn $ renderTagsOptions ropts $ strip tags
{% endhighlight %}

Example output: 

    $ runhaskell Detag.hs 
    > {-# LANGUAGE RankNTypes          #-}
    > {-# LANGUAGE RankNTypes          #-}
    
    ...
