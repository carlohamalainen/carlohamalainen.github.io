---
author: Carlo Hamalainen

date: "2008-11-09T00:00:00Z"
format: image
title: Cython vs. C++, improved
url: /2008/11/09/cython-vs-c-improved/
---
In a [recent post](http://carlo-hamalainen.net/blog/2008/03/04/cython-vs-c/) I compared my Cython and C++ implementations of a depth first search algorithm. The Cython code was quite slow, and Robert Bradshaw commented:

> This graph looked pretty depressing, so I made some optimizations to your code (basically the ones suggested above, and a couple of other glaring things that stood out). The algorithm is still completely the same, and I didn’t do any code re-factoring other than ``__getitem__``/``__setitem__``, just mostly typing things here and there. It’s now faster than c++ on my machine for the whole range graphed above (and much faster for small inputs).
> 
> Code and diff up at <http://sage.math.washington.edu/home/robertwb/cython/scratch/cython-latin/>

I applied Robert's patch and re-ran the tests on my laptop:

![](/stuff/myfiles/cython-vs-cpp-new.png) 

The Cython implementation with Robert's patch is now significantly faster than the C++ implementation on most of the range that I checked.

