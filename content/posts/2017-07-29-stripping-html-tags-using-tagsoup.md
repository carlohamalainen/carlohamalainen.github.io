---
author: Carlo Hamalainen
date: "2017-07-29T09:32:47Z"
title: Stripping html tags using TagSoup
url: /2017/07/29/stripping-html-tags-using-tagsoup/
---
I had a situation, when converting old blog posts to WordPress, where I wanted to strip all the extra info on the `pre` tags. For example this:

    <pre class="brush: plain; title: ; notranslate" title=""><pre><code><span style="">></span> <span style="color: blue; font-weight: bold;">import</span><span style="">Data</span><span style="">.</span><span style="">Char</span>

would turn into: 

    > import Data.Char

It turns out that this is really easy using [TagSoup](https://hackage.haskell.org/package/tagsoup).

```
module Detag where

import Control.Monad
import Text.HTML.TagSoup
```

The function to strip tags works on a list of tags of strings: 

```
strip :: [Tag String] -> [Tag String]
strip [] = []
```

If we hit a `pre` tag, ignore its info (the underscore) and continue on recursively: 

```
strip (TagOpen "pre" _ : rest) = TagOpen "pre" [] : strip rest
```

Similarly, strip the info off an opening code tag: 

```
strip (TagOpen  "code" _ : rest) = strip rest
strip (TagClose "code"   : rest) = strip rest
```

If we hit a span, followed by some text, and a closing span, then keep the text tag and continue: 

```
strip (TagOpen "span" _ : TagText t : TagClose "span" : rest)
  = TagText t : strip rest
```

Don't change other tags: 

```
strip (t:ts) = t : strip ts
```

Parsing input from stdin is straightforward. We use `optEscape` and `optRawTag` to avoid mangling other html in the input.

```
main :: IO ()
main = do
    s <- getContents
    let tags = parseTags s
        ropts = renderOptions{optEscape = id, optRawTag = const True}
    putStrLn $ renderTagsOptions ropts $ strip tags
```

Example output: 

    $ runhaskell Detag.hs 
    > {-# LANGUAGE RankNTypes          #-}
    > {-# LANGUAGE RankNTypes          #-}
    
    ...
