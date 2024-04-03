---
author: Carlo Hamalainen
date: "2009-06-18T00:00:00Z"
format: image
title: Debugging Sage using Eric4
url: /2009/06/18/debugging-sage-using-eric4/
---
This is just a short note on how to use the Eric4 program to debug a Sage script (a .py file). It's a slightly updated version of a post on some Sage mailing list that I can't find right now. 

On Ubuntu 9.04, install Eric4:

    sudo apt-get install eric

Save the following to a file:

    #!/bin/bash
    x=`pwd`
    export SAGE_ROOT=/path_to_your_sage_installation
    cd $SAGE_ROOT
    source local/bin/sage-env
    cd $x
    sage-python ${*}

Start Eric4 and go to Settings, Preferences, Debugger/Python, and set the Custom Python Interpreter to the file that you just saved.

Using File, Open you can debug a Python file that uses the Sage libraries, e.g.:

```python
from sage.all import *
r=MPolynomialRing(GF(127),2,'x')
print r.gens()
```

To set a breakpoint in the Sage library itself use the ? sign to find out where a file is located. For example

    sage: PolynomialRing?

shows us that ``PolynomialRing`` comes from

    $SAGE_ROOT/local/lib/python2.5/site-packages/sage/rings/polynomial/polynomial_ring_constructor.py

If you open that file (File, Open) you can set breakpoints and so on.

A note to netbook users: Eric4's default window is quite large and did not display properly on my Asus EEE PC. The solution is to remove most of the toolbar entries (these are accessible from the menu anyway) and also change the window mode to detached windows so that you can alt-tab between the various components of Eric4 instead of having them all in one giant window.
