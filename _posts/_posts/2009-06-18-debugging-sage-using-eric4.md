---
id: 742
title: Debugging Sage using Eric4
date: 2009-06-18T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2009/06/18/debugging-sage-using-eric4/
permalink: /2009/06/18/debugging-sage-using-eric4/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
This is just a short note on how to use the Eric4 program to debug a Sage script (a .py file). It&#8217;s a slightly updated version of a post on some Sage mailing list that I can&#8217;t find right now. 

On Ubuntu 9.04, install Eric4:

<pre>sudo apt-get install eric</pre>

Save the following to a file:

<pre>#!/bin/bash
x=`pwd`
export SAGE_ROOT=/path_to_your_sage_installation
cd $SAGE_ROOT
source local/bin/sage-env
cd $x
sage-python ${*}</pre>

Start Eric4 and go to Settings, Preferences, Debugger/Python, and set the Custom Python Interpreter to the file that you just saved.

Using File, Open you can debug a Python file that uses the Sage libraries, e.g.:

<pre>from sage.all import *
r=MPolynomialRing(GF(127),2,'x')
print r.gens()</pre>

To set a breakpoint in the Sage library itself use the ? sign to find out where a file is located. For example

<pre>sage: PolynomialRing?</pre>

shows us that PolynomialRing comes from

<pre>$SAGE_ROOT/local/lib/python2.5/site-packages/sage/rings/polynomial/polynomial_ring_constructor.py
</pre>

If you open that file (File, Open) you can set breakpoints and so on.

A note to netbook users: Eric4&#8217;s default window is quite large and did not display properly on my Asus EEE PC. The solution is to remove most of the toolbar entries (these are accessible from the menu anyway) and also change the window mode to detached windows so that you can alt-tab between the various components of Eric4 instead of having them all in one giant window.