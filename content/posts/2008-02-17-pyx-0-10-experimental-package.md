---
author: Carlo Hamalainen
date: "2008-02-17T00:00:00Z"
format: image
guid: http://carlo-hamalainen.net/2008/02/17/pyx-0-10-experimental-package/
id: 829
original_post_id:
- "16"
restapi_import_id:
- 596a05ef0330b
title: pyx-0.10 experimental package
url: /2008/02/17/pyx-0-10-experimental-package/
---
[PyX](http://pyx.sourceforge.net/) is a Python package for the creation of PostScript and PDF files. It seems like a more modern alternative to [MetaPost](http://en.wikipedia.org/wiki/MetaPost) so I was keen to try it out. I found that the experimental pyx-0.8.1 package in Sage 2.10 did not work (errors about a DVI file not finishing) so I created a Sage source package of the latest version: [pyx-0.10.spkg](http://carlo-hamalainen.net/sage/pyx-0.10.spkg).

Download that package and then do ``sage -i pyx-0.10.skpg`` to install it.

Here is the standard Hello World example (look for ``hello.pdf`` in your working directory):

```python
import pyx

x = y = int(0)

c = pyx.canvas.canvas()  
c.text(x, y, "Hello, world!")  
c.stroke(pyx.path.line(x, y, x+5, y+0))  
c.writePDFfile("hello")
```

The previous pyx spkg tried to put the ``pyxrc`` file into ``/etc`` but I prefer to run Sage as a normal user, and sudo-ing to install a package isn't completely straightforward (you have to set some environment variables, so it's not newbie-friendly). I made my spkg-install file put pyxrc into ``$SAGE_LOCAL/etc`` but I'm not sure if this is a suitable location. Any suggestions?

edit: On the sage-devel mailing list I was told to use ``~/.sage`` so I have ammended the pyx-0.10 package to do this.
