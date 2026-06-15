---
author: Carlo Hamalainen
categories:
date: "2007-12-11T00:00:00Z"
format: image
title: Installing Minion Pro fonts
url: /2007/12/11/installing-minion-pro-fonts/
---
### Introduction

This post describes how to install Minion Pro fonts. It was originally written for Ubuntu 7.04/7.10 but seems to work on systems as new as Debian Wheezy, ElementaryOS, and Linux Mint 15. It should work for any modern Linux distribution with TexLive. Some readers have been able to adapt these instructions for OSX (see the comments at the end of this post and also <http://jklukas.blogspot.com/2010/02/installing-minionpro-tex-package.html>). Also, someone has [translated this post into Japanese](http://watermans-linuxtips.blogspot.com/2009/01/minion-prolatex.html). Cool 🙂 And here is a shell script: <https://github.com/jonkeane/MinionProforLaTeX>. 

We will install to /usr/local/share/texmf. Check that this matches the definition of TEXMFLOCAL in your installation: 

```shell-session
$ kpsexpand '$TEXMFLOCAL'
/usr/local/share/texmf
```

It is vital that you install to the right path in ``/usr/local``. 

### Install mnsymbol

Click on [mnsymbol](http://www.ctan.org/tex-archive/fonts/mnsymbol) and download the entire directory as ``mnsymbol.zip``. 

```shell-session
$ unzip mnsymbol.zip
$ cd mnsymbol/tex
$ latex MnSymbol.ins
$ mkdir -p /usr/local/share/texmf/tex/latex/MnSymbol/
$ mkdir -p /usr/local/share/texmf/fonts/source/public/MnSymbol/
$ mkdir -p /usr/local/share/texmf/doc/latex/MnSymbol/
$ cp MnSymbol.sty /usr/local/share/texmf/tex/latex/MnSymbol/MnSymbol.sty
$ cd ..
$ cp source/* /usr/local/share/texmf/fonts/source/public/MnSymbol/
$ cp MnSymbol.pdf README /usr/local/share/texmf/doc/latex/MnSymbol/
$ mkdir -p /usr/local/share/texmf/fonts/map/dvips/MnSymbol
$ mkdir -p /usr/local/share/texmf/fonts/enc/dvips/MnSymbol
$ mkdir -p /usr/local/share/texmf/fonts/type1/public/MnSymbol
$ mkdir -p /usr/local/share/texmf/fonts/tfm/public/MnSymbol
$ cp enc/MnSymbol.map /usr/local/share/texmf/fonts/map/dvips/MnSymbol/
$ cp enc/*.enc /usr/local/share/texmf/fonts/enc/dvips/MnSymbol/
$ cp pfb/*.pfb /usr/local/share/texmf/fonts/type1/public/MnSymbol/
$ cp tfm/* /usr/local/share/texmf/fonts/tfm/public/MnSymbol/
```

Regenerate the indexes and enable MnSymbol: 

```shell-session
$ mktexlsr
$ updmap-sys --enable MixedMap MnSymbol.map
```

You should be able to compile [mnsymbol-test.tex](/stuff/myfiles/minionpro/mnsymbol-test.tex) with no errors. 

### Install the MinionPro package

We will need a few files from here: <http://www.ctan.org/tex-archive/fonts/minionpro/>. 

Download ``scripts.zip`` and unpack it: 

```shell-session
$ mkdir minionpro-scripts
$ cd minionpro-scripts
$ unzip ../scripts.zip
```

Copy your OTF fonts into the local directory. 

```shell-session
$ find /youradobefonts/ -iname '*minion*pro*otf' -exec cp -v '{}' otf/ ';'
```

Hint: Adobe Reader ships with some Minion Pro fonts. 

Make sure you have the latest version of lcdf-typetools and then convert the fonts: 

```shell-session
$ sudo apt-get install lcdf-typetools
$ ./convert.sh
```

Install the fonts: 

```shell-session
$ mkdir -p /usr/local/share/texmf/fonts/type1/adobe/MinionPro/
$ cp pfb/*.pfb /usr/local/share/texmf/fonts/type1/adobe/MinionPro/
```

Determine which version of the Adobe fonts you have. For example, I have the "002.000" family: 

```shell-session
$ otfinfo -v ~/Desktop/minionpro-scripts/otf/MinionPro-Regular.otf
Version 2.015;PS 002.000;Core 1.0.38;makeotf.lib1.7.9032
```

You need to download **one** of the following encoding files: 

```
Version | Encoding file
------------------------
001.000 | enc-v1.000.zip
001.001 | enc-v1.001.zip
002.000 | env-v2.000.zip
```

The last few steps: 

```shell-session
$ cd /usr/local/share/texmf
$ unzip ~/Desktop/metrics-base.zip
$ unzip ~/Desktop/metrics-full.zip
$ unzip ~/Desktop/enc-X.XXX.zip        (pick your version)
```

Edit ``/etc/texmf/updmap.d/10local.cfg`` and add the following line: 

```
Map MinionPro.map
```

Regenerate all indexes: 

```shell-session
$ mktexlsr
$ update-updmap
$ updmap-sys
```

You should see a line like this: 

```
updmap-sys: using map file `/usr/local/share/texmf/fonts/map/dvips/MnSymbol/MnSymbol.map'
```

You should now be able to compile [minionpro-test.tex](/stuff/myfiles/minionpro/minionpro-test.tex) with no errors. 

You might see this error on large documents: 

```
Font OMS/MnSymbolS/m/n/17.28=MnSymbolS12 at 17.28pt not loaded: Not enough room left.
```

On texlive-2009 systems, you may be able to edit ``/etc/texmf/texmf.d/95NonPath.cnf`` and change 

```
font_mem_size = 500000
```

to 

```
font_mem_size = 5000000
```

or some other large value. Then run

```shell-session
$ update-texmf
```

I'm not sure about newer versions of TexLive, e.g. <http://tug.org/pipermail/tex-live/2012-November/032677.html>.

