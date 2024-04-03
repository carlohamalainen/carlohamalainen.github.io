---
author: Carlo Hamalainen

date: "2014-01-09T00:00:00Z"
format: image
guid: http://carlo-hamalainen.net/2014/01/09/improving-the-haskell-toolchain-jump-to-haddock/
title: 'Improving the Haskell toolchain: jump to haddock'
url: /2014/01/09/improving-the-haskell-toolchain-jump-to-haddock/
---
Recently on [haskell-cafe](http://www.haskell.org/pipermail/haskell-cafe/2013-December/111778.html) I asked if there is an easy way to get the Haddock documentation url for a given identifier in Haskell. I discovered that it's not completely straightforward, for example if we look up String using ghc-pkg we are told that it's defined in GHC.Base:

```
$ ghc-mod info foo.hs Foo String
type String = [Char]     -- Defined in `GHC.Base'
```

Then we can find the Haddock url for the base package:

```$ ghc-pkg find-module GHC.Base
/home/carlo/opt/ghc-7.6.3_build/lib/ghc-7.6.3/package.conf.d
   base-4.6.0.1
/home/carlo/.ghc/x86_64-linux-7.6.3/package.conf.d

$ ghc-pkg field base-4.6.0.1 haddock-html
haddock-html: /home/carlo/opt/ghc-7.6.3_build/share/doc/ghc/html/libraries/base-4.6.0.1
```

so we'd expect to be able to view

```
/home/carlo/opt/ghc-7.6.3_build/share/doc/ghc/html/libraries/base-4.6.0.1/GHC-Base.html
```

but this file doesn't exist because GHC.Base is an
internal module and does not have a Haddock page. This happens with some other packages too, not just GHC.Base.

In the haskell-cafe thread, [Roman Cheplyaka](http://ro-che.info/) pointed out that I should be asking for where a thing is _imported from_, as opposed to _defined_. For the String example we would prefer to send the user to the Haddock page for the Prelude.

This turns out to be a bit tricky as well, but I think I have a reasonable prototype working now, which I have unimaginatively called [ghc-imported-from](https://github.com/carlohamalainen/ghc-imported-from). There are some very rough edges, for example parsing of language extensions and GHC options, but these should be easy to fix either by following how [ghc-mod](http://www.mew.org/~kazu/proj/ghc-mod/en/) does the same thing, or perhaps by adding my code to a fork of ghc-mod. We'll see.

Here's a short screencast of ghc-imported-from in action, along with my forked version of ghcmod-vim:

{{< youtube VVc8uupYJGs >}}

 **Make sure that the video plays fullscreen in 720p otherwise the text is illegible.**
