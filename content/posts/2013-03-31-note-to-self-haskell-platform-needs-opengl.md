---
author: Carlo Hamalainen

date: "2013-03-31T00:00:00Z"
format: image
guid: http://carlo-hamalainen.net/2013/03/31/note-to-self-haskell-platform-needs-opengl/
title: 'Note to self: haskell-platform needs OpenGL'
url: /2013/03/31/note-to-self-haskell-platform-needs-opengl/
---
On Debian squeeze, one needs some OpenGL libraries to install haskell-platform-2012.4.0.0: 

```
✗ 09:42:08 carlo@r500 ~/opt/haskell-platform-2012.4.0.0 $ ./configure --prefix=/home/carlo/opt/haskell-platform-2012.4.0.0_build

...

checking zlib.h usability... yes
checking zlib.h presence... yes
checking for zlib.h... yes
checking for zlibVersion in -lz... yes
checking GL/gl.h usability... no
checking GL/gl.h presence... no
checking for GL/gl.h... no
configure: error: The OpenGL C library is required
```

Solution: 

```
sudo apt-get install libgl1-mesa-dev                    
                     libglc-dev                         
                     freeglut3-dev                      
                     libedit-dev                        
                     libglw1-mesa libglw1-mesa-dev
```

Credit: <http://nathanwiegand.com/blog/2009/07/haskell-platform-on-ubuntu.html>
