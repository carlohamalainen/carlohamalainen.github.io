---
id: 751
title: SciPy, _ZNSt8ios_base4InitD1Ev, and link flags
date: 2012-03-14T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2012/03/14/scipy-_znst8ios_base4initd1ev-and-link-flags/
permalink: /2012/03/14/scipy-_znst8ios_base4initd1ev-and-link-flags/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
I recently tried to build SciPy 0.10.1 on a system with both the GNU and Intel compilers. Everything went find except that import scipy bombed out with:

<pre>ImportError: /opt/scipy/0.10.1/lib/python2.7/site-packages/scipy/
sparse/sparsetools/_csr.so: undefined symbol: _ZNSt8ios_base4InitD1Ev</pre>

The fix is to add &#8220;-lstdc++&#8221; to the link flags. I found a post on the [SciPy mailing list](http://mail.scipy.org/pipermail/scipy-user/2010-March/024523.html) where someone had the same problem and asked &#8220;Could someone please advise me how to ensure that the &#8220;-lstdc++&#8221; is successfully passed to the linker as and when I build scipy.&#8221;

The answer is to use the build_ext target to enable the link flag:

<pre>python setup.py config --compiler=intel --cc=icc --fcompiler=intelem build_ext -lstdc++
python setup.py config --compiler=intel --fcompiler=intelem install --prefix=/opt/scipy/0.10.1
</pre>

**Archived Comments**

Date: 2012-07-26 11:00:02 UTC

Author: Charles

Thank you for this reposting. It was extremely useful (e.g. solved my problem)!