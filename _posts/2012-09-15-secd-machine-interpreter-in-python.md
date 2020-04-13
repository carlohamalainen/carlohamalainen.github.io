---
id: 807
title: SECD machine interpreter in Python
date: 2012-09-15T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2012/09/15/secd-machine-interpreter-in-python/
permalink: /2012/09/15/secd-machine-interpreter-in-python/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
I wrote a Python interpreter for the SECD abstract machine, following the presentation in [Kogge's book](http://carlo-hamalainen.net/blog/2012/08/15/kogges-the-architecture-of-symbolic-computers-1991/). The [pydot](http://code.google.com/p/pydot/) library was convenient for visualising the graph in memory corresponding to data and code. For example this short program: 

    [LDC, [3, 4], LDF, [LD, [1, 2], LD, [1, 1], ADD, RTN], AP, WRITEI, STOP,]

looks like this (click for enlarged image): 

[<img src="https://github.com/carlohamalainen/pysecd/raw/master/program_in_memory.png?w=640&ssl=1"  data-recalc-dims="1" />](https://github.com/carlohamalainen/pysecd/raw/master/program_in_memory.png?ssl=1)

Source code is available on github: <https://github.com/carlohamalainen/pysecd>
