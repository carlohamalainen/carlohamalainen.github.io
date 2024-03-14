---
author: Carlo Hamalainen

date: "2012-07-06T00:00:00Z"
format: image
guid: http://carlo-hamalainen.net/2012/07/06/passing-a-numpy-array-to-a-c-function/
id: 773
original_post_id:
- "16"
restapi_import_id:
- 596a05ef0330b
title: Passing a NumPy array to a C function
url: /2012/07/06/passing-a-numpy-array-to-a-c-function/
---
Just a note to myself: <http://stackoverflow.com/questions/3046305/simple-wrapping-of-c-code-with-cython> has a nice self-contained example of passing a NumPy array to C. And some comments about being aware of non-contiguous arrays or Fortran indexing. 

I used this example at work recently to optimize a nested inner for-loop in a statistical bootstrap program and the Python/Cython/C version runs 15 times faster than the plain Python code. The [GNU Scientific Library](http://www.gnu.org/software/gsl/) has some convenient [random number generators](http://www.gnu.org/software/gsl/manual/html_node/Random-Number-Generation.html).