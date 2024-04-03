---
author: Carlo Hamalainen

date: "2012-09-15T00:00:00Z"
format: image
guid: http://carlo-hamalainen.net/2012/09/15/secd-machine-interpreter-in-python/
title: SECD machine interpreter in Python
url: /2012/09/15/secd-machine-interpreter-in-python/
---
I wrote a Python interpreter for the SECD abstract machine, following the presentation in [Kogge's book](/2012/08/15/kogges-the-architecture-of-symbolic-computers-1991/). The [pydot](http://code.google.com/p/pydot/) library was convenient for visualising the graph in memory corresponding to data and code. For example this short program: 

    [LDC, [3, 4], LDF, [LD, [1, 2], LD, [1, 1], ADD, RTN], AP, WRITEI, STOP,]

looks like this: 

{{< figure src="https://github.com/carlohamalainen/pysecd/raw/master/program_in_memory.png" >}}

Source code is available on github: <https://github.com/carlohamalainen/pysecd>
