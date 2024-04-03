---
author: Carlo Hamalainen

date: "2009-09-15T00:00:00Z"
format: image
guid: http://carlo-hamalainen.net/2009/09/15/compiling-gambit-from-sourceforge-on-ubuntu-9-04/
title: Compiling gambit from sourceforge on Ubuntu 9.04
url: /2009/09/15/compiling-gambit-from-sourceforge-on-ubuntu-9-04/
---
Here are notes on compiling gambit revision 6294 (the current latest) from the [SourceForge gambit repository](http://gambit.svn.sourceforge.net/viewvc/gambit/).

There is [a problem](http://programphases.com/forums/showthread.php?p=29) with libtool on Ubuntu 8 and 9, that will cause the build process to fail with an error like this:

    ../../ libtool: ....    X--tag=CXX: command not found

The only workaround is to use an older version of libtool from Ubuntu hardy. Here is how to install it:

    sudo aptitude purge libtool
    wget http://mirrors.kernel.org/ubuntu/pool/main/libt/libtool/libtool_1.5.26-1ubuntu1_i386.deb
    sudo dpkg -i libtool_1.5.26-1ubuntu1_i386.deb

It is helpful to "pin" this package so that it won't be automatically updated when running apt-get dist-upgrade. Edit /etc/apt/preferences (if the file doesn't exist then just create it) and write this:

    Package: libtool
    Pin: version 1.5.26-1ubuntu1
    Pin-Priority: 1001

This will keep libtool at version 1.5.26-1ubuntu1.

Now to install the latest version of gambit:

    sudo aptitude install autoconf autotools-dev svn automake
    svn co https://gambit.svn.sourceforge.net/svnroot/gambit gambit
    cd gambit/trunk/gambit/
    aclocal
    autoconf
    autoheader
    automake

The autoheader command is probably not necessary but I'm mentioning it because it's part of the usual process to make a GNU configure script.

Now make and install as usual:

    $ ./configure --prefix=$HOME/gambit
    $ make
    $ make install

To run gambit add these lines to $HOME/.bashrc:

    export LD_LIBRARY_PATH=$HOME/gambit/lib/
    export PATH=$PATH:$HOME/gambit/bin/

Open a terminal and run gambit.
