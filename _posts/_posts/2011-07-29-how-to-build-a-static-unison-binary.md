---
id: 732
title: How to build a static Unison binary
date: 2011-07-29T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2011/07/29/how-to-build-a-static-unison-binary/
permalink: /2011/07/29/how-to-build-a-static-unison-binary/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
The current documentation for [Unison](http://www.cis.upenn.edu/~bcpierce/unison/) (as at 2.32.52) is slightly out of date, because the STATIC=true option to the Makefile does not do anything. The fix is to grab the patch file from [this post](http://permalink.gmane.org/gmane.network.unison.general/7879) and then apply it as follows:

<pre>cd unison-2.32.52
patch -p0 &lt; ../patch-unison-static
make STATIC=true
</pre>

Very handy if you have to run unison on a system with an old glibc.