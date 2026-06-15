---
author: Carlo Hamalainen

date: "2008-03-04T00:00:00Z"
format: image
title: Cython vs C++
url: /2008/03/04/cython-vs-c/
---
Edit (2008-11-09): Robert Bradshaw posted a patch to my code and the Cython implementation is now a lot faster. Click [here](/2008/11/09/cython-vs-c-improved/) to read more.

In a comment on a [recent post](/2007/12/18/speeding-up-code-using-cython/), Robert Samal asked how Cython compares to C++. The graph below shows a comparison of a greedy critical set solver written in Cython and C++ (both use a brute force, naive, non-randomised implementation of a depth first search): 

![](/stuff/myfiles/cython-vs-cpp.png) 

So things look good until n = 10. In defence of Cython, I must point out that my implementation was a first attempt and I am by no means an expert on writing good Cython code. Also, the Cython code is probably fast enough -- in my experience, solving problems (computationally) for latin squares of order 10 is futile, so the code is more convenient for testing out small ideas.

edit: the code is [here](http://carlo-hamalainen.net/sage/latin-1.2/)

edit: Robert's code is here <http://sage.math.washington.edu/home/robertwb/cython/scratch/cython-latin/>

