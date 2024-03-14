---
author: Carlo Hamalainen
date: "2020-04-25T19:00:00Z"
title: PuTTY for netrw on Vim and Neovim on Windows 7
url: /2020/04/25/vim-neovim/
---

Update 2024-02: I wouldn't bother doing any of this, I'd just use
[vscode-remote](https://github.com/Microsoft/vscode-remote-release) in VS Code. Easy!


----------

Vim and Neovim come with the netrw plugin for editing remote files. The way I use this
is to write a list of frequently used files in my scratch file for a project, and go to
edit the file by putting the cursor over the line and hitting ``gf``:

```console
scp://carlo@192.168.1.9//home/carlo/foo-from-pscp.txt
```

I also have a Vim leader command to quickly open my scratch file:

```vim
map <Leader>0 :e /home/carlo/0.txt<CR>
```

Editing remote files is straightforward on Linux but on Windows 7 one has to use PuTTY's pscp. In an 
enterprise environment there seem to be group policy settings that interfere with the default shell
(or maybe my Cygwin environment does something - I can't work out what the difference is).

First set up PuTTY, then set Vim's ``_vimrc`` or Neovim's ``init.vim``.

## PuTTY Session

Create a new Session. I called mine ``server``:

{{< figure src="/stuff/2020-04-25-vim-neovim/putty-settings.png" >}}

Set the username to login with:

{{< figure src="/stuff/2020-04-25-vim-neovim/putty-settings-connection-data.png" >}}
 
Set the private key (use PuTTYGen to create your key):

{{< figure src="/stuff/2020-04-25-vim-neovim/putty-settings-connection-SSH-Auth.png" >}}

Test pscp in a command shell. Use ``-load server`` to load the Session.

```console
C:\Users\carlo>pscp -load server foo.txt carlo@192.168.1.9:/home/carlo/foo-from-pscp.txt
foo.txt                   | 0 kB |   0.0 kB/s | ETA: 00:00:00 | 100%
```

## Vim 8.2 configuration

On Windows the vimrc file is in a path like ``C:\Users\<user>\_vimrc``.

```vim
" This file is C:\Users\carlo\_vimrc

syntax on
set incsearch

let g:netrw_cygwin = 0
let g:netrw_scp_cmd = "\"C:\\Program Files\\PuTTY\\pscp.exe\" -load server"

" I want to edit this remote file using 'gf'
" scp://carlo@192.168.1.9//home/carlo/foo-from-pscp.txt
```

## Neovim 0.4.3 configuration

Neovim's equivalent of vimrc is a file 
like ``C:\Users\<user>\AppData\Local\nvim\init.vim``

```vim
" This file is C:\Users\carlo\AppData\Local\nvim\init.vim

syntax on

set incsearch

let g:netrw_cygwin = 0
let g:netrw_scp_cmd = "\"C:\\Program Files\\PuTTY\\pscp.exe\" -load server"

" I want to edit this remote file using 'gf'
" scp://carlo@192.168.1.9//home/carlo/foo-from-pscp.txt
```

## Neovim 0.4.3 configuration on an enterprise system

The same version of Neovim on an old Windows 7 work laptop failed to edit remote files. It kept
complaining about not being able to start ``/bin/bash``. To get it to work there
I had to force the shell to ``cmd.exe`` and use a slightly different scp command. No idea why.

```vim
let shell=cmd.exe
let g:netrw_cygwin = 0
let g:netrw_scp_cmd = 'C:\"Program Files"\PuTTY\pscp.exe -batch -q -i C:\work\ssh\ssh-putty-laptop.ppk -scp'
```

## Further reading

* [Vim: you don't need NERDtree or (maybe) netrw](https://shapeshed.com/vim-netrw/)
* [Why does NerdTree exist? What’s wrong with netrw?](https://www.reddit.com/r/vim/comments/22ztqp/why_does_nerdtree_exist_whats_wrong_with_netrw/)
* [netrw command reference](https://gist.github.com/danidiaz/37a69305e2ed3319bfff9631175c5d0f)
* [Use one big text file (first entry)](http://n-gate.com/hackernews/2020/02/14/0/)
