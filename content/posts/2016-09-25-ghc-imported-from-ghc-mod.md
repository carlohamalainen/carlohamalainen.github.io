---
author: Carlo Hamalainen

date: "2016-09-25T00:00:00Z"
format: image
guid: http://carlo-hamalainen.net/2016/09/25/ghc-imported-from-ghc-mod/
title: ghc-imported-from => ghc-mod
url: /2016/09/25/ghc-imported-from-ghc-mod/
---
 **This post has some errors; see [ghc-imported-from--ghc-mod-march-2017](/2017/03/19/ghc-imported-from--ghc-mod-march-2017) for the latest instructions.** 

I have a [pull request](https://github.com/DanielG/ghc-mod/pull/823) to merge [ghc-imported-from](https://hackage.haskell.org/package/ghc-imported-from) into [ghc-mod](https://github.com/DanielG/ghc-mod). The main benefit of being part of ghc-mod is that I don't have to duplicate ghc-mod's infrastructure for handling sandboxes, GHC options, interfaces to other build tools like Stack, and compatibility with more versions of GHC.

The pull request is still under review, so until then you can try it out by cloning the development branches: 

```
git clone -b imported-from https://github.com/DanielG/ghc-mod.git ghc-mod-imported-from
cd ghc-mod-imported-from
cabal update && cabal sandbox init && cabal install
export PATH=`pwd`/.cabal-sandbox/bin:$PATH
```

Assuming that you use [Plugged](https://github.com/junegunn/vim-plug) for managing  
Vim/Neovim plugins, also grab my branch of ghcmod-vim: 

```
git clone -b ghcmod-imported-from-cmd git@github.com:carlohamalainen/ghcmod-vim.git $HOME/.vim/plugged/ghcmod-vim
```

and make sure that the plugin is enabled: 

```
call plug#begin('~/.vim/plugged')
Plug 'eagletmt/ghcmod-vim', {'for': 'haskell'}
```

and set some handy key mappings: 

```
au FileType  haskell nnoremap            :GhcModType
au FileType  haskell nnoremap            :GhcModInfo
au FileType  haskell nnoremap    :GhcModTypeClear

au FileType lhaskell nnoremap            :GhcModType
au FileType lhaskell nnoremap            :GhcModInfo
au FileType lhaskell nnoremap    :GhcModTypeClear

au FileType haskell  nnoremap   :GhcModOpenDoc
au FileType lhaskell nnoremap   :GhcModOpenDoc

au FileType haskell  nnoremap   :GhcModDocUrl
au FileType lhaskell nnoremap   :GhcModDocUrl

au FileType haskell  vnoremap  : GhcModOpenHaddockVismode
au FileType lhaskell vnoremap  : GhcModOpenHaddockVismode

au FileType haskell  vnoremap  : GhcModEchoUrlVismode
au FileType lhaskell vnoremap  : GhcModEchoUrlVismode
```

On the command line, use the imported-from command. It tells you the defining module, the exporting module, and the Haddock URL: 

```
$ ghc-mod imported-from Foo.hs 9 34 show
base-4.8.2.0:GHC.Show.show Prelude https://hackage.haskell.org/package/base-4.8.2.0/docs/Prelude.html
```

From Vim/Neovim, navigate to a symbol and hit F4 which will open the Haddock URL in your browser, or F5 to echo the command-line output. 

{{< figure src="/s3/oldblog/blogdata/x-2016-09/ghc-imported-from-url.png" >}}
