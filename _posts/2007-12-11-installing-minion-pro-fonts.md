---
id: 687
title: Installing Minion Pro fonts
date: 2007-12-11T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2007/12/11/installing-minion-pro-fonts/
permalink: /2007/12/11/installing-minion-pro-fonts/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
### Introduction

This post describes how to install Minion Pro fonts. It was originally written for Ubuntu 7.04/7.10 but seems to work on systems as new as Debian Wheezy, ElementaryOS, and Linux Mint 15. It should work for any modern Linux distribution with TexLive. Some readers have been able to adapt these instructions for OSX (see the comments at the end of this post and also <http://jklukas.blogspot.com/2010/02/installing-minionpro-tex-package.html>). Also, someone has [translated this post into Japanese](http://watermans-linuxtips.blogspot.com/2009/01/minion-prolatex.html). Cool 🙂 And here is a shell script: <https://github.com/jonkeane/MinionProforLaTeX>. 

We will install to /usr/local/share/texmf. Check that this matches the definition of TEXMFLOCAL in your installation: 

<pre>$ kpsexpand '$TEXMFLOCAL'
/usr/local/share/texmf
</pre>

It is vital that you install to the right path in <tt>/usr/local</tt>. 

### Install mnsymbol

Click on [mnsymbol](http://www.ctan.org/tex-archive/fonts/mnsymbol) and download the entire directory as <tt>mnsymbol.zip</tt>. 

<pre>unzip mnsymbol.zip
cd mnsymbol/tex
latex MnSymbol.ins
mkdir -p /usr/local/share/texmf/tex/latex/MnSymbol/
mkdir -p /usr/local/share/texmf/fonts/source/public/MnSymbol/
mkdir -p /usr/local/share/texmf/doc/latex/MnSymbol/
cp MnSymbol.sty /usr/local/share/texmf/tex/latex/MnSymbol/MnSymbol.sty
cd ..
cp source/* /usr/local/share/texmf/fonts/source/public/MnSymbol/
cp MnSymbol.pdf README /usr/local/share/texmf/doc/latex/MnSymbol/
mkdir -p /usr/local/share/texmf/fonts/map/dvips/MnSymbol
mkdir -p /usr/local/share/texmf/fonts/enc/dvips/MnSymbol
mkdir -p /usr/local/share/texmf/fonts/type1/public/MnSymbol
mkdir -p /usr/local/share/texmf/fonts/tfm/public/MnSymbol
cp enc/MnSymbol.map /usr/local/share/texmf/fonts/map/dvips/MnSymbol/
cp enc/*.enc /usr/local/share/texmf/fonts/enc/dvips/MnSymbol/
cp pfb/*.pfb /usr/local/share/texmf/fonts/type1/public/MnSymbol/
cp tfm/* /usr/local/share/texmf/fonts/tfm/public/MnSymbol/
</pre>

Regenerate the indexes and enable MnSymbol: 

<pre>mktexlsr
updmap-sys --enable MixedMap MnSymbol.map
</pre>

You should be able to compile [mnsymbol-test.tex](https://s3.amazonaws.com/carlo-hamalainen.net/oldblog/stuff/myfiles/minionpro/mnsymbol-test.tex) with no errors. 

### Install the MinionPro package

We will need a few files from here: <http://www.ctan.org/tex-archive/fonts/minionpro/>. 

Download <tt>scripts.zip</tt> and unpack it: 

<pre>mkdir minionpro-scripts
cd minionpro-scripts
unzip ../scripts.zip
</pre>

Copy your OTF fonts into the local directory. 

<pre>find /youradobefonts/ -iname '*minion*pro*otf' -exec cp -v '{}' otf/ ';'
</pre>

Hint: Adobe Reader ships with some Minion Pro fonts. 

Make sure you have the latest version of lcdf-typetools and then convert the fonts: 

<pre>sudo apt-get install lcdf-typetools
./convert.sh
</pre>

Install the fonts: 

<pre>mkdir -p /usr/local/share/texmf/fonts/type1/adobe/MinionPro/
cp pfb/*.pfb /usr/local/share/texmf/fonts/type1/adobe/MinionPro/
</pre>

Determine which version of the Adobe fonts you have. For example, I have the "002.000" family: 

<pre>$ otfinfo -v ~/Desktop/minionpro-scripts/otf/MinionPro-Regular.otf
Version 2.015;PS 002.000;Core 1.0.38;makeotf.lib1.7.9032
</pre>

You need to download **one** of the following encoding files: 

<pre>Version | Encoding file
------------------------
001.000 | enc-v1.000.zip
001.001 | enc-v1.001.zip
002.000 | env-v2.000.zip
</pre>

The last few steps: 

<pre>cd /usr/local/share/texmf
unzip ~/Desktop/metrics-base.zip
unzip ~/Desktop/metrics-full.zip
unzip ~/Desktop/enc-X.XXX.zip        (pick your version)
</pre>

Edit <tt>/etc/texmf/updmap.d/10local.cfg</tt> and add the following line: 

<pre>Map MinionPro.map
</pre>

Regenerate all indexes: 

<pre>mktexlsr
update-updmap
updmap-sys
</pre>

You should see a line like this: 

<pre>updmap-sys: using map file `/usr/local/share/texmf/fonts/map/dvips/MnSymbol/MnSymbol.map'
</pre>

You should now be able to compile [minionpro-test.tex](https://s3.amazonaws.com/carlo-hamalainen.net/oldblog/stuff/myfiles/minionpro/minionpro-test.tex) with no errors. 

You might see this error on large documents: 

<pre>Font OMS/MnSymbolS/m/n/17.28=MnSymbolS12 at 17.28pt not loaded: Not enough room left.
</pre>

On texlive-2009 systems, you may be able to edit <tt>/etc/texmf/texmf.d/95NonPath.cnf</tt> and change 

<pre>font_mem_size = 500000
</pre>

to 

<pre>font_mem_size = 5000000
</pre>

or some other large value. Then run

<pre>update-texmf
</pre>

I'm not sure about newer versions of TexLive, e.g. <http://tug.org/pipermail/tex-live/2012-November/032677.html>.

**Archived Comments**

Date: 2008-11-23 05:59:10 UTC

Author: Marcello

I wanted to install minionpro fonts in order to use them  
in my thesis document.  
Your instructions are great and simplify alot a quite complicated procedure. Thank you very much Carlo!!!

Date: 2009-02-06 00:34:09 UTC

Author: Marc

Hi from Paris/France,  
Thanks a lot for this tutorial. I strictly followed your instructions and everything went fine.  
My system : Debian Lenny on 386 and a classic latex installation via aptitude.  
On debian, Adobe fonts are in /usr/lib/Adobe, so it's on this directory I had to "find".  
Real great job ! Thx !  
Marc

Date: 2009-04-06 10:17:06 UTC

Author: Alex

Dear Carlo,

it works like a charm! Thanks a lot!

Mail me your address, i'll send you some selfmade wine.

Thank you so much!  
Alex

Date: 2009-05-12 09:01:18 UTC

Author: Noel Dela Cruz

thanks

Date: 2009-05-12 09:02:01 UTC

Author: Noel Dela Cruz

This is great hope to used it regularly

Date: 2009-06-11 06:02:02 UTC

Author: svat

Thanks a lot for this detailed tutorial! Although I'm using Mac OS X with TexLive (MacTeX) 2008, it worked perfectly for me, with only minor modifications:  
* I used ~Library/texmf instead of /usr/local/share/texmf everywhere  
* Some of the commands required sudo  
* There was no update-updmap, nor was there a /etc/texmf/updmap.d/, but running "sudo updmap-sys --enable Map=MinionPro.map" did the trick.

I'm amazed by how well it all worked; thanks again for the perfectly clear instructions!

Date: 2009-07-27 09:32:56 UTC

Author: John

Thanks! Very clear!  
Some people might also need to unzip the opticals file (I think).  
Oh, for the mac it's "- -- enable" (there two dashes there).

Date: 2009-09-15 13:03:58 UTC

Author: lydia

I am more or less a Latex noob and am not that familiar to working with the terminal. I use mac os x. I managed to do the copying but when I try to do the mtexlsr (what's that for anyways?) it echoes command not found, the same with udpmap-sys.  
thx for your help!

Date: 2009-12-22 11:39:17 UTC

Author: Jamie Bullock

I also used these fonts for my PhD thesis, and this tutorial was a lifesaver! Many many thanks!

Date: 2011-12-04 22:06:31 UTC

Author: Thomas

I still don'T know how to use it in my latex document.

usepackage{minion} doesn't work.

-----

I ran latex on a file called 'MinionPro.dtx'. Or was it 'MinionPro.ins'? Anyway, it was contained in the MinionPro Source.Zip Package from CTAN. Namely, in they subdirectory called "tex".

Date: 2013-03-14 02:01:50 UTC

Author: Dane

Thanks for this write-up. When you link to mnsymbol, you should leave off the trailing slash, otherwise the link to download it as a zip archive isn't available (CTAN's server is a little finicky)

Date: 2013-03-14 07:24:08 UTC

Author: carlo

Dane: fixed, thanks. It looks like CTAN changed their web design recently. Never noticed the trailing-slash issue before.

Date: 2013-10-29 12:49:32.326749 UTC

Author: John

Also Carlo, if you are updating anyway i found a small change. updmap has been updated and running update-updmap returns that it is an outdated process and should use updmap-sys instead. It does include it still but says to expect inconsistencies.

Thanks for the great tutorial, really, really, really thankful.

Date: 2013-11-28 10:34:08.960239 UTC

Author: Sascha

Thanks for the guide! You saved my live.

Just a small update:

I use Ubuntu 13.10 and /etc/texmf/updmap.d/10local.cfg does not exist anymore. I created it but update-updmap throws the following warning: 

Old configuration style found in /etc/texmf/updmap.d

For now these files have been included, but expect inconsistencies.

In the end it worked for me but maybe one should edit the file /var/lib/texmf/updmap.cfg-DEBIAN instead (on Ubuntu 13.10).

Also, I never saw this line: updmap-sys: using map file \`/usr/local/share/texmf/fonts/map/dvips/MnSymbol/MnSymbol.map'

Date: 2015-03-28 05:49:23.528116 UTC

Author: exl

Everything works to the point of

sudo mktexlsr

Then when I try the last command, I get this error:

updmap: resetting $HOME value (was /Users/xx) to root's actual home (/var/root).  
updmap is using the following updmap.cfg files (in precedence order):  
/var/root/Library/texlive/2014/texmf-config/web2c/updmap.cfg  
/usr/local/texlive/2014/texmf-config/web2c/updmap.cfg  
/usr/local/texlive/2014/texmf-dist/web2c/updmap.cfg  
updmap is using the following updmap.cfg file for writing changes:  
/var/root/Library/texlive/2014/texmf-config/web2c/updmap.cfg  
/var/root/Library/texlive/2014/texmf-config/web2c/updmap.cfg unchanged. Map files not recreated.  
updmap: Updating ls-R files.  
[Macintosh-25:texlive/2014/texmf-config] xxlin%  
[Macintosh-25:texlive/2014/texmf-config] xxlin%

So close, but not working. Any guidance would be much appreciated!