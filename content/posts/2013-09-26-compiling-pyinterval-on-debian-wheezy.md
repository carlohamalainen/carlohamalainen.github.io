---
author: Carlo Hamalainen

date: "2013-09-26T00:00:00Z"
format: image
title: Compiling pyinterval on Debian Wheezy
url: /2013/09/26/compiling-pyinterval-on-debian-wheezy/
---
When installing ``pyinterval`` on my Debian Wheezy system via ``pip install pyinterval`` I hit this:

```
/usr/bin/ld: /usr/local/lib/libcrlibm.a(crlibm_private.o): relocation R_X86_64_32 against `.rodata.str1.8' can not be used when making a shared object; recompile with -fPIC

/usr/local/lib/libcrlibm.a: could not read symbols: Bad value

collect2: error: ld returned 1 exit status

error: command 'gcc' failed with exit status 1
```

The solution is to install crlibm with the -fPIC flag:

```
tar zxf crlibm-1.0beta4.tar.gz
cd crlibm-1.0beta4

export CPPFLAGS=-fPIC
./configure
make
sudo make install

sudo pip install pyinterval
```

Then you should be able to run Rump's example (see Stefano Taschini's SciPy 2008 paper on pyinterval): 

```
carlo@x1 ~ $ python
Python 2.7.3 (default, Jan  2 2013, 13:56:14)
[GCC 4.7.2] on linux2
Type "help", "copyright", "credits" or "license" for more information.
>>> from interval import interval
>>> def f(x,y):
...     return (
...         (333.75 - x**2)* y**6 + x**2 *
...             (11* x**2 * y**2 - 121 * y**4 - 2)
...         + 5.5 * y**8 + x/(2*y))
...
>>> f(interval(77617.0), interval(33096.0))
interval([-3.541774862152234e+21, 3.5417748621522344e+21])
>>>
```
