---
id: 738
title: 'Installation notes: wxHaskell on Debian Squeeze'
date: 2013-01-18T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2013/01/18/installation-notes-wxhaskell-on-debian-squeeze/
permalink: /2013/01/18/installation-notes-wxhaskell-on-debian-squeeze/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
As of January 2013 I was not able to find Debian packages for wxWidgets 2.9, which is required by 0.90.0.1, so we&#8217;ll install directly from source. First grab some dependencies: 

<pre>sudo apt-get install build-essential libgl1-mesa-dev libglu1-mesa-dev freeglut3-dev
</pre>

Download 2.9.4 from <http://www.wxwidgets.org/downloads/>. Then: 

<pre>tar jxf wxWidgets-2.9.4.tar.bz2
cd wxWidgets-2.9.4
./configure --open-gl
make -j 10
sudo make install
</pre>

This installs to /usr/local. Make sure that wx-config reports the correct libs, e.g. 

<pre>$ wx-config  --libs
-L/usr/local/lib -pthread   -lwx_gtk2u_xrc-2.9 -lwx_gtk2u_html-2.9 -lwx_gtk2u_qa-2.9 -lwx_gtk2u_adv-2.9 -lwx_gtk2u_core-2.9 -lwx_baseu_xml-2.9 -lwx_baseu_net-2.9 -lwx_baseu-2.9
</pre>

Now try to install using cabal: 

<pre>cabal update
cabal install wx
</pre>

If the build fails with

<pre>/usr/local/include/wx-2.9/wx/gtk/bitmap.h:64:24: error:   initializing argument 1 of ‘wxBitmap& wxBitmap::operator=(const wxBitmap&)’
</pre>

then we need to patch a C++ file in wxc (credit for this is due to Mads Lindstrøm on [comp.lang.haskell.wxhaskell.general](http://comments.gmane.org/gmane.comp.lang.haskell.wxhaskell.general/1277)). 

<pre>cd ~/.cabal/packages/hackage.haskell.org/wxc/0.90.0.4
tar jxf wxc-0.90.0.4.tar.gz
</pre>

Now edit wxc-0.90.0.4/src/cpp/eljpen.cpp and change line 159 from 

<pre>* _ref = NULL;
</pre>

to

<pre>_ref = NULL;
</pre>

Then repackage: 

<pre>tar zcvf wxc-0.90.0.4.tar.gz wxc-0.90.0.4
rm -fr wxc-0.90.0.4
</pre>

Beware that the patch to wxc is under cabal&#8217;s control, so it could be lost if wxc is upgraded, etc. Finally, try to install again using: 

<pre>cabal install wx
</pre>

Finally, make sure that the [hello world](http://www.haskell.org/haskellwiki/WxHaskell/Quick_start#Hello_world_in_wxHaskell) example works: 

<pre>module Main where
import Graphics.UI.WX

main :: IO ()
main = start hello

hello :: IO ()
hello = do
    f    &lt;- frame    [text := "Hello!"]
    quit &lt;- button f [text := "Quit", on command := close f]
    set f [layout := widget quit]
</pre>

Note: if cabal install wx fails with

<pre>src/cpp/glcanvas.cpp:43:60: error: ‘wxGLContext’ has not been declared
src/cpp/glcanvas.cpp:102:1: error: ‘wxGLContext’ does not name a type
src/cpp/glcanvas.cpp:109:1: error: ‘wxGLContext’ does not name a type
src/cpp/glcanvas.cpp:116:1: error: ‘wxGLContext’ was not declared in this scope
src/cpp/glcanvas.cpp:116:1: error: ‘self’ was not declared in this scope
src/cpp/glcanvas.cpp:116:1: error: expected primary-expression before ‘void’
src/cpp/glcanvas.cpp:116:1: error: expression list treated as compound expression in initializer [-fpermissive]
src/cpp/glcanvas.cpp:117:1: error: expected ‘,’ or ‘;’ before ‘{’ token
cabal: Error: some packages failed to install:
</pre>

then your system is missing OpenGL libraries (this is not a bug in wx). Double-check the configure output of wxWidgets. 

**Archived Comments**

Date: 2013-03-19 09:24:45 UTC

Author: babalone

Very helpful. I tried installing wx a few times and failed until now.

Date: 2013-03-27 02:30:14 UTC

Author: Hermit

Thanks, however, I&#8217;m still unable to build wxcore. It&#8217;s spurting a lot of compiler errors: <a href="http://dpaste.com/1035609" rel="nofollow">http://dpaste.com/1035609</a>

When running wx-config &#8211;libs, -lwx\_gtk2u\_html-2.9 is there so&#8230;

I&#8217;ve been banging my head against a wall with this. I&#8217;m not very into C/C++ stack (that&#8217;s why I use haskell!!). I couldn&#8217;t build it for OSX either&#8230;

I ended up switching toolkits, moved to HTk, but it&#8217;s orphan so I wonder if it&#8217;s worth it for the long run.

Date: 2014-10-25 01:05:27.99669 UTC

Author: david diggles

Having similar issues on openSUSE. I wish cabal was better with package management.