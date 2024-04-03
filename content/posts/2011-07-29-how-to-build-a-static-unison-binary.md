---
author: Carlo Hamalainen

date: "2011-07-29T00:00:00Z"
format: image
title: How to build a static Unison binary
url: /2011/07/29/how-to-build-a-static-unison-binary/
---
The current documentation for [Unison](http://www.cis.upenn.edu/~bcpierce/unison/) (as at 2.32.52) is slightly out of date, because the STATIC=true option to the Makefile does not do anything. The fix is to grab the patch file from [this post](http://permalink.gmane.org/gmane.network.unison.general/7879) and then apply it as follows:

    cd unison-2.32.52
    patch -p0 < ../patch-unison-static
    make STATIC=true

Very handy if you have to run unison on a system with an old glibc.
