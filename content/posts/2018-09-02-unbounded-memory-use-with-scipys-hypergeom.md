---
author: Carlo Hamalainen
date: "2018-09-02T08:56:49Z"
title: Unbounded memory use with SciPy's hypergeom
url: /2018/09/02/unbounded-memory-use-with-scipys-hypergeom/
---
**Edit 2019-01-12: fixed here: [numpy/pull/11977](https://github.com/numpy/numpy/pull/11977)**

[Nadiah](https://nadiah.org/) ran into an apparent memory leak in SciPy's [hypergeom](https://docs.scipy.org/doc/scipy-1.1.0/reference/generated/scipy.stats.hypergeom.html#scipy.stats.hypergeom) distribution.

Here is a minimal example. Running this code results in unbounded memory use, looking like a memory leak:

```python
from scipy.stats import hypergeom

while True:
    x = hypergeom(100, 30, 40).cdf(3)
```

It turns out that this isn't really a memory leak but rather a problem with NumPy's vectorize method which creates a circular reference in some situations. Here's the GitHub issue that I opened: [numpy/issues/11867](https://github.com/numpy/numpy/issues/11867).

In the mean time, a workaround is to manually delete the `_ufunc` attribute after using `cdf`:

```python
from scipy.stats import hypergeom

while True:
    h = hypergeom(100, 30, 40)
    x = h.cdf(3)
    del h.dist._cdfvec._ufunc
```

Alternatively, avoid the frozen distribution and call `cdf` directly:

```python
from scipy.stats import hypergeom

while True:
    x = hypergeom.cdf(3, 100, 30, 40)
```

It's worth mentioning that [memory_profiler](https://pypi.org/project/memory_profiler/) is a great tool for finding memory leaks:

```python
from scipy.stats import hypergeom
from memory_profiler import profile

@profile
def main1():
    for _ in range(1000):
        x = hypergeom(100, 30, 40).cdf(3)

main1()
```

Output:

    $ python3 geomprofile.py 
    Filename: geomprofile.py

    Line #    Mem usage    Increment   Line Contents
    ================================================
         4     69.2 MiB     69.2 MiB   @profile
         5                             def main1():
         6     79.2 MiB      0.0 MiB       for _ in range(1000):
         7     79.2 MiB     10.0 MiB           x = hypergeom(100, 30, 40).cdf(3)

We see that the `hypergeom` line contributed to an increase in memory use of 10Mb.

Drilling down into NumPy's `vectorize` took a bit of manual debugging; I didn't have as much luck with memory_profiler there.

In a production situation one might not have the luxury of finding the real cause of the memory leak immediately. In that case it might be enough to wrap the offending code in a call to [multiprocessing](https://docs.python.org/3/library/multiprocessing.html) so that the leaked memory is reclaimed frequently. A lightweight option is to use [processify](https://gist.github.com/schlamar/2311116). See [Liau Yung Siang's blog post](https://ys-l.github.io/posts/2015/10/03/processifying-bulky-functions-in-python) for more details.
