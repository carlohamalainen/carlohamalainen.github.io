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

**Archived Comments**

Date: 2008-03-04 05:40:32 UTC

Author: Mike Hansen

You should post the Cython and C++ code because it looks like there maybe some obvious fixes to the Cython to make it behave better.

Date: 2008-03-04 21:01:39 UTC

Author: Robert Samal

Does somebody else have some experience in how cython compares  
with C/C++? Every once in a while I need to do some computation (something NP-complete or worse in general, so it  
usually ends up as an ugly backtracking). I'd be happy to do everything from within Sage (and using python/cython), but I'm not sure, if it is fast enough (or if it getting fast enough, I suppose that cython is improving gradually).

Date: 2008-07-07 11:59:22 UTC

Author: Alexandre Delattre

Hi,

After looking quickly into the code, I'm pretty sure some overhead is caused by the ``__getitem__`` and ``__setitem__`` methods, you use to override the [] operator.

When calling ``L[i, j]`` (or ``L[i, j] = x``), those special methods are resolved at runtime and hence involve additional python mechanism. While they make the code readable, you lose the interest of "cdef" methods which are called much faster.

IMO, a good compromise would be to put the code in ``__getitem__`` into a regular 'cdef getitem()' method, then make ``__getitem__`` as a wrapper of the regular method:

```python
    def __getitem__(self, rc):  
    i, j = rc  
    return self.getitem(i, j)

    cdef int getitem(int i, int j):  
    ... # Put your code here
```

and replace the ``L[i, j]`` by ``L.getitem(i, j)`` in your cython code.

Also put "void" return type on cdef method that returns nothing could help a bit.

I'll try to make these changes and run the benchmark again.

Date: 2008-11-08 15:12:21 UTC

Author: Robert Bradshaw

This graph looked pretty depressing, so I made some optimizations to your code (basically the ones suggested above, and a couple of other glaring things that stood out). The algorithm is still completely the same, and I didn't do any code re-factoring other than ``__getitem__``/``__setitem__``, just mostly typing things here and there. It's now faster than c++ on my machine for the whole range graphed above (and much faster for small inputs).

Code and diff up at [http://sage.math.washington.edu/home/robertwb/cython-latin/"](http://sage.math.washington.edu/home/robertwb/cython-latin/")

Date: 2008-11-11 17:24:47 UTC

Author: Ben Racine

Any chance that we might see a plot of the improved data... wouldn't want people to come here and only see the 'depressing' data.

Date: 2008-11-11 17:27:12 UTC

Author: Ben Racine

Nevermind, I now see the new results up one level.

Date: 2011-09-14 02:19:56 UTC

Author: Alex Quinn

The link to the improved data is dead:  <http://carlo-hamalainen.net/blog/?p=35>

Same for the link to the motivation ("recent post"): <http://carlo-hamalainen.net/blog/?p=12a>

Are these viewable elsewhere?

Thanks a lot for doing this and posting it! Very helpful in any case.

Date: 2011-09-14 02:22:59 UTC

Author: Alex Quinn

Found it! Here's the post with the improved data: <https://carlo-hamalainen.net/2008/11/09/cython-vs-c-improved/>

Date: 2011-09-14 03:47:40 UTC

Author: Alex Quinn

Code link is still broken:  
<http://sage.math.washington.edu/home/robertwb/cython-latin/>

Date: 2015-10-10 08:12:16.866201 UTC

Author: Mohammad M. Shahbazi

nice challenge. I've been using cython for a couple of years. it really sucks
