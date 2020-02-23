---
id: 726
title: Compiling pyinterval on Debian Wheezy
date: 2013-09-26T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2013/09/26/compiling-pyinterval-on-debian-wheezy/
permalink: /2013/09/26/compiling-pyinterval-on-debian-wheezy/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
When installing pyinterval on my Debian Wheezy system via pip install pyinterval I hit this:

<pre>/usr/bin/ld: /usr/local/lib/libcrlibm.a(crlibm_private.o): relocation R_X86_64_32 against `.rodata.str1.8' can not be used when making a shared object; recompile with -fPIC

/usr/local/lib/libcrlibm.a: could not read symbols: Bad value

collect2: error: ld returned 1 exit status

error: command 'gcc' failed with exit status 1
</pre>

The solution is to install crlibm with the -fPIC flag:

<pre>tar zxf crlibm-1.0beta4.tar.gz
cd crlibm-1.0beta4

export CPPFLAGS=-fPIC
./configure
make
sudo make install

sudo pip install pyinterval
</pre>

Then you should be able to run Rump&#8217;s example (see Stefano Taschini&#8217;s SciPy 2008 paper on pyinterval): 

<pre>carlo@x1 ~ $ python
Python 2.7.3 (default, Jan  2 2013, 13:56:14)
[GCC 4.7.2] on linux2
Type "help", "copyright", "credits" or "license" for more information.
&gt;&gt;&gt; from interval import interval
&gt;&gt;&gt; def f(x,y):
...     return (
...         (333.75 - x**2)* y**6 + x**2 *
...             (11* x**2 * y**2 - 121 * y**4 - 2)
...         + 5.5 * y**8 + x/(2*y))
...
&gt;&gt;&gt; f(interval(77617.0), interval(33096.0))
interval([-3.541774862152234e+21, 3.5417748621522344e+21])
&gt;&gt;&gt;
</pre>