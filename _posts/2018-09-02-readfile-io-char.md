---
id: 1245
title: 'readFile :: String -> IO [Char]'
date: 2018-09-02T05:37:48+00:00
author: Carlo Hamalainen
layout: post
guid: https://carlo-hamalainen.net/?p=1245
permalink: /2018/09/02/readfile-io-char/
inline_featured_image:
  - "0"
categories:
  - Uncategorized
---
Using a modern GHC compiler, how much memory would this program use?

<pre class="brush: plain; title: ; notranslate" title="">x &lt;- readFile "foo"
x `deepseq` print ()
</pre>

Linear in the size of foo? Or something else?

Turns out, for the default `readFile` from the Prelude, the answer is about 40 times the size of the input file.

The default Haskell strings take 5 words per character, so on a 64bit machine this is 40 5*8 = 40 bytes per character. The list of characters is stored as a linked list, roughly like this diagram (taken from [Johan Tibbel&#8217;s ZuriHac 2015 talk](https://www.youtube.com/watch?reload=9&v=_pDUq0nNjhI), slides are [here](https://github.com/tibbe/talks/blob/master/zurihac-2015/slides.md)):

<img src="https://i1.wp.com/raw.githubusercontent.com/carlohamalainen/playground/master/ghc-memory-usage/zurihac2015-johan-tibbel/intpair.png?w=1100&#038;ssl=1" data-recalc-dims="1" /> 

We can check the actual memory usage of a Haskell program (compiled with GHC) by using the RTS options:

<pre class="brush: plain; title: ; notranslate" title="">ghc readfile.hs -Wall -O2 -rtsopts

./readfile +RTS -toutput &lt;other options&gt;
</pre>

See [this repository](https://github.com/carlohamalainen/playground/tree/master/ghc-memory-usage) for some scripts to benchmark a few variants of `readFile`:

  * Prelude
  * Data.ByteString
  * Data.ByteString.Char8
  * Data.ByteString.Lazy
  * Data.ByteString.Lazy.Char8

Basically, anything&#8217;s better than the default `readFile`:

<img src="https://i1.wp.com/carlo-hamalainen.net/wp-content/uploads/2018/08/readfile_memory_usage.png?resize=640%2C480&#038;ssl=1" class="alignnone size-full wp-image-1246" alt="readfile_memory_usage" width="640" height="480" srcset="https://i1.wp.com/carlo-hamalainen.net/wp-content/uploads/2018/08/readfile_memory_usage.png?w=640&ssl=1 640w, https://i1.wp.com/carlo-hamalainen.net/wp-content/uploads/2018/08/readfile_memory_usage.png?resize=300%2C225&ssl=1 300w" sizes="(max-width: 640px) 100vw, 640px" data-recalc-dims="1" />