---
id: 778
title: Compiling gambit-0.2007.12.04 on Ubuntu 9.04
date: 2009-09-10T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2009/09/10/compiling-gambit-0-2007-12-04-on-ubuntu-9-04/
permalink: /2009/09/10/compiling-gambit-0-2007-12-04-on-ubuntu-9-04/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
The stable release 2007.12.04 of the game theory package [Gambit](http://gambit.sourceforge.net/) does not compile out of the box on Ubuntu 9.04. The problem is just a few missing #include lines, probably because the gcc header files have been updated since 2007.

Here is a patch: [gambit-0.2007.12.04-ubuntu-9.04-patch](http://carlo-hamalainen.net/stuff/gambit-0.2007.12.04-ubuntu-9.04-patch). To use the patch do the following.

Unpack a fresh copy of Gambit:

<pre>tar zxf gambit-0.2007.12.04.tar.gz
</pre>

Save the patch in the same directory; apply the patch:

<pre>cd gambit-0.2007.12.04
patch -p1 &lt; ../gambit-0.2007.12.04-ubuntu-9.04-patch
</pre>

Now configure and make as usual:

<pre>./configure
make
make install
</pre>

**Archived Comments**

Date: 2009-09-15 16:44:23 UTC

Author: Ryan

Very helpful &#8212; thanks!