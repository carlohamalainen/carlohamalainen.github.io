---
author: Carlo Hamalainen

date: "2017-08-17T10:35:19Z"
title: ghc-imported-from => ghc-mod (August 2017)
url: /2017/08/17/ghc-imported-from-ghc-mod-august-2017/
---
I have a [pull request](https://github.com/DanielG/ghc-mod/pull/823) to merge [ghc-imported-from](https://hackage.haskell.org/package/ghc-imported-from) into [ghc-mod](https://github.com/DanielG/ghc-mod). The main benefit of being part of ghc-mod is that I don't have to duplicate ghc-mod's infrastructure for handling sandboxes, GHC options, interfaces to other build tools like Stack, and compatibility with more versions of GHC.

The pull request is still under review, so until then you can try it out by cloning the development branches:

```
git clone -b imported-from https://github.com/DanielG/ghc-mod.git ghc-mod-imported-from
cd ghc-mod-imported-from
cabal update && cabal sandbox init && cabal install
export PATH=`pwd`/.cabal-sandbox/bin:$PATH
```

Assuming that you use [Plugged](https://github.com/junegunn/vim-plug) for managing Vim/Neovim plugins, use my branch of ghcmod-vim by adding this to your vimrc:
```
call plug#begin('~/.vim/plugged')

Plug 'carlohamalainen/ghcmod-vim', { 'branch': 'ghcmod-imported-from-cmd', 'for' : 'haskell' }
```

Install the plugin with :PlugInstall in vim.

Recently, `xdg-open` stopped working for me (others have had [the same issue](https://unix.stackexchange.com/questions/237250/how-can-i-open-all-urls-in-my-browser-with-xdg-open)) so I recommend setting `ghcmod_browser` in your vimrc:

```
let g:ghcmod_browser = '/usr/bin/firefox'
```

Here are some handy key mappings:

```
au FileType  haskell nnoremap  :GhcModType
au FileType  haskell nnoremap  :GhcModInfo
au FileType  haskell nnoremap  :GhcModTypeClear

au FileType lhaskell nnoremap  :GhcModType
au FileType lhaskell nnoremap  :GhcModInfo
au FileType lhaskell nnoremap  :GhcModTypeClear

au FileType haskell  nnoremap  :GhcModOpenDoc
au FileType lhaskell nnoremap  :GhcModOpenDoc

au FileType haskell  nnoremap  :GhcModDocUrl
au FileType lhaskell nnoremap  :GhcModDocUrl

au FileType haskell  vnoremap  :GhcModOpenHaddockVismode
au FileType lhaskell vnoremap  :GhcModOpenHaddockVismode

au FileType haskell  vnoremap  :GhcModEchoUrlVismode
au FileType lhaskell vnoremap  :GhcModEchoUrlVismode
```

On the command line, use the imported-from command. It tells you the defining module, the exporting module, and the Haddock URL:

```
$ ghc-mod imported-from Foo.hs 9 34
base-4.8.2.0:System.IO.print Prelude /opt/ghc/7.10.3/share/doc/ghc/html/libraries/base-4.8.2.0/Prelude.html
```

From Vim/Neovim, navigate to a symbol and hit F4 which will open the Haddock URL in your browser, or F5 to echo the command-line output.
