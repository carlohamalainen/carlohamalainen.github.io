---
author: Carlo Hamalainen

date: "2012-03-14T00:00:00Z"
format: image
title: SciPy, _ZNSt8ios_base4InitD1Ev, and link flags
url: /2012/03/14/scipy-_znst8ios_base4initd1ev-and-link-flags/
---
I recently tried to build SciPy 0.10.1 on a system with both the GNU and Intel compilers. Everything went find except that import scipy bombed out with:

    ImportError: /opt/scipy/0.10.1/lib/python2.7/site-packages/scipy/
    sparse/sparsetools/_csr.so: undefined symbol: _ZNSt8ios_base4InitD1Ev

The fix is to add "-lstdc++" to the link flags. I found a post on the [SciPy mailing list](http://mail.scipy.org/pipermail/scipy-user/2010-March/024523.html) where someone had the same problem and asked "Could someone please advise me how to ensure that the "-lstdc++" is successfully passed to the linker as and when I build scipy."

The answer is to use the ``build_ext`` target to enable the link flag:

    python setup.py config --compiler=intel --cc=icc --fcompiler=intelem build_ext -lstdc++
    python setup.py config --compiler=intel --fcompiler=intelem install --prefix=/opt/scipy/0.10.1


**Archived Comments**

Date: 2012-07-26 11:00:02 UTC

Author: Charles

Thank you for this reposting. It was extremely useful (e.g. solved my problem)!
