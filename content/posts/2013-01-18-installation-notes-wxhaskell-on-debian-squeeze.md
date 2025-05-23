---
author: Carlo Hamalainen

date: "2013-01-18T00:00:00Z"
format: image
title: 'Installation notes: wxHaskell on Debian Squeeze'
url: /2013/01/18/installation-notes-wxhaskell-on-debian-squeeze/
---
As of January 2013 I was not able to find Debian packages for wxWidgets 2.9, which is required by 0.90.0.1, so we'll install directly from source. First grab some dependencies:

```
sudo apt-get install build-essential libgl1-mesa-dev libglu1-mesa-dev freeglut3-dev
```

Download 2.9.4 from <http://www.wxwidgets.org/downloads/>. Then:

```
tar jxf wxWidgets-2.9.4.tar.bz2
cd wxWidgets-2.9.4
./configure --open-gl
make -j 10
sudo make install
```

This installs to /usr/local. Make sure that wx-config reports the correct libs, e.g.

```
$ wx-config  --libs
-L/usr/local/lib -pthread   -lwx_gtk2u_xrc-2.9 -lwx_gtk2u_html-2.9 -lwx_gtk2u_qa-2.9 -lwx_gtk2u_adv-2.9 -lwx_gtk2u_core-2.9 -lwx_baseu_xml-2.9 -lwx_baseu_net-2.9 -lwx_baseu-2.9
```

Now try to install using cabal:

```
cabal update
cabal install wx
```

If the build fails with

```
/usr/local/include/wx-2.9/wx/gtk/bitmap.h:64:24: error:   initializing argument 1 of ‘wxBitmap& wxBitmap::operator=(const wxBitmap&)’
```

then we need to patch a C++ file in wxc (credit for this is due to Mads Lindstrøm on [comp.lang.haskell.wxhaskell.general](http://comments.gmane.org/gmane.comp.lang.haskell.wxhaskell.general/1277)).

```
cd ~/.cabal/packages/hackage.haskell.org/wxc/0.90.0.4
tar jxf wxc-0.90.0.4.tar.gz
```

Now edit wxc-0.90.0.4/src/cpp/eljpen.cpp and change line 159 from

```
* _ref = NULL;
```

to

```
_ref = NULL;
```

Then repackage:

```
tar zcvf wxc-0.90.0.4.tar.gz wxc-0.90.0.4
rm -fr wxc-0.90.0.4
```

Beware that the patch to wxc is under cabal's control, so it could be lost if wxc is upgraded, etc. Finally, try to install again using:

```
cabal install wx
```

Finally, make sure that the [hello world](http://www.haskell.org/haskellwiki/WxHaskell/Quick_start#Hello_world_in_wxHaskell) example works:

```
module Main where
import Graphics.UI.WX

main :: IO ()
main = start hello

hello :: IO ()
hello = do
    f    <- frame    [text := "Hello!"]
    quit <- button f [text := "Quit", on command := close f]
    set f [layout := widget quit]
```

Note: if cabal install wx fails with

```
src/cpp/glcanvas.cpp:43:60: error: ‘wxGLContext’ has not been declared
src/cpp/glcanvas.cpp:102:1: error: ‘wxGLContext’ does not name a type
src/cpp/glcanvas.cpp:109:1: error: ‘wxGLContext’ does not name a type
src/cpp/glcanvas.cpp:116:1: error: ‘wxGLContext’ was not declared in this scope
src/cpp/glcanvas.cpp:116:1: error: ‘self’ was not declared in this scope
src/cpp/glcanvas.cpp:116:1: error: expected primary-expression before ‘void’
src/cpp/glcanvas.cpp:116:1: error: expression list treated as compound expression in initializer [-fpermissive]
src/cpp/glcanvas.cpp:117:1: error: expected ‘,’ or ‘;’ before ‘{’ token
cabal: Error: some packages failed to install:
```

then your system is missing OpenGL libraries (this is not a bug in wx). Double-check the configure output of wxWidgets.

**Archived Comments**

Date: 2013-03-19 09:24:45 UTC

Author: babalone

Very helpful. I tried installing wx a few times and failed until now.

Date: 2013-03-27 02:30:14 UTC

Author: Hermit

Thanks, however, I'm still unable to build wxcore. It's spurting a lot of compiler errors: <http://dpaste.com/1035609>

When running wx-config --libs, -lwx\_gtk2u\_html-2.9 is there so...

I've been banging my head against a wall with this. I'm not very into C/C++ stack (that's why I use haskell!!). I couldn't build it for OSX either...

I ended up switching toolkits, moved to HTk, but it's orphan so I wonder if it's worth it for the long run.

Date: 2014-10-25 01:05:27.99669 UTC

Author: david diggles

Having similar issues on openSUSE. I wish cabal was better with package management.
