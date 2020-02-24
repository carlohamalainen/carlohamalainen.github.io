---
id: 799
title: Compiling gambit from sourceforge on Ubuntu 9.04
date: 2009-09-15T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2009/09/15/compiling-gambit-from-sourceforge-on-ubuntu-9-04/
permalink: /2009/09/15/compiling-gambit-from-sourceforge-on-ubuntu-9-04/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
Here are notes on compiling gambit revision 6294 (the current latest) from the [SourceForge gambit repository](http://gambit.svn.sourceforge.net/viewvc/gambit/).

There is [a problem](http://programphases.com/forums/showthread.php?p=29) with libtool on Ubuntu 8 and 9, that will cause the build process to fail with an error like this:

<pre>../../ libtool: ....    X--tag=CXX: command not found
</pre>

The only workaround is to use an older version of libtool from Ubuntu hardy. Here is how to install it:

<pre>$ sudo aptitude purge libtool
$ wget http://mirrors.kernel.org/ubuntu/pool/main/libt/libtool/libtool_1.5.26-1ubuntu1_i386.deb
$ sudo dpkg -i libtool_1.5.26-1ubuntu1_i386.deb
</pre>

It is helpful to "pin" this package so that it won't be automatically updated when running apt-get dist-upgrade. Edit /etc/apt/preferences (if the file doesn't exist then just create it) and write this:

<pre>Package: libtool
Pin: version 1.5.26-1ubuntu1
Pin-Priority: 1001
</pre>

This will keep libtool at version 1.5.26-1ubuntu1.

Now to install the latest version of gambit:

<pre>$ sudo aptitude install autoconf autotools-dev svn automake
$ svn co https://gambit.svn.sourceforge.net/svnroot/gambit gambit
$ cd gambit/trunk/gambit/
$ aclocal
$ autoconf
$ autoheader
$ automake
</pre>

The autoheader command is probably not necessary but I'm mentioning it because it's part of the usual process to make a GNU configure script.

Now make and install as usual:

<pre>$ ./configure --prefix=$HOME/gambit
$ make
$ make install
</pre>

To run gambit add these lines to $HOME/.bashrc:

<pre>export LD_LIBRARY_PATH=$HOME/gambit/lib/
export PATH=$PATH:$HOME/gambit/bin/
</pre>

Open a terminal and run gambit.