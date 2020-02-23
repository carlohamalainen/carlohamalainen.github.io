---
id: 822
title: Speeding up code using Cython
date: 2007-12-18T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2007/12/18/speeding-up-code-using-cython/
permalink: /2007/12/18/speeding-up-code-using-cython/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
The graph below compares a brute-force depth first search in Sage/Python (red line) to an implementation of the same algorithm in Cython (blue line). Vertical axis is run time in seconds, horizontal axis is number of latin square completions generated:

![](myfiles/py_v_pyrex.png) 

Cython does pretty well, and [the code](http://carlo-hamalainen.net/sage/latin-1.0/) is far more readable than the earlier C++ version that I wrote.

For more about writing Cython code in Sage, see [Chapter 5](http://sagemath.org/doc/html/prog/node32.html) of the documentation.

**Archived Comments**

Date: 2008-03-03 18:38:36 UTC

Author: Robert Samal

Hi, thanks for the interesting comparison.  
How does Cython compare to C/C++?

Thanks,

Robert Samal