---
author: Carlo Hamalainen

date: "2008-03-01T00:00:00Z"
format: image
guid: http://carlo-hamalainen.net/2008/03/01/an-exact-cover-solver-for-sage/
title: An exact cover solver for Sage
url: /2008/03/01/an-exact-cover-solver-for-sage/
---
For a while now I've been using my own C++ implementation of Knuth's [Dancing Links](http://en.wikipedia.org/wiki/Dancing_Links) algorithm. The algorithm is quite fast for solving the 0-1 matrix exact cover problem. More importantly, a number of other combinatorial problems can be converted into an exact cover framework, such as finding latin square completions, enumerating latin bitrades, and computing the chromatic number and chromatic polynomial of graphs (those last two are from [comments](http://groups.google.com/group/sage-devel/browse_thread/thread/8e1172f7772052f/67993b71c60bdd14?lnk=gst&q=exact+cover#67993b71c60bdd14) by Tom Boothby on the sage-devel mailing list).

The thread started by Tom inspired me to make my code public, license it with GPL, write a few doctests, and learn the necessary Cython to write a wrapper for my C++ code so that it would be usable from Sage. The code is available [here](http://carlo-hamalainen.net/sage/latin-1.1/), in particular ``dancing_links.sage`` (top level Sage wrapper), ``dancing_links.spyx`` (Cython code that calls my C++ code), and finally dancing_links.cpp (the actual solver).

Not surprisingly, the C++ version is a fair bit faster than the Python implementation adapted by Tom when used as a latin square solver for finding "greedy" critical sets:

![](/stuff/myfiles/dlx-timing.png) 

x-axis: \(n\), y-axis: number of seconds to compute the greedy critical set of \(B_n\) (addition table for \(Z_n\)), blue is Python, red is C++.
