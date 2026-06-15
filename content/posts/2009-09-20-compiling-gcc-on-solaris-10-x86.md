---
author: Carlo Hamalainen

date: "2009-09-20T00:00:00Z"
format: image
title: Compiling gcc on Solaris 10 x86
url: /2009/09/20/compiling-gcc-on-solaris-10-x86/
---
Here are step by step instructions for compiling gcc 4.2.4 on a fresh installation of Solaris 10 x86. Some key information came from [a post by Andreas Höschler](http://www.mail-archive.com/discuss-gnustep@gnu.org/msg11010.html) on the discuss-gnustep mailing list.

First you will need to get pkgutil (more info [here](http://pkgutil.wikidot.com/get-install-and-configure#toc3)). Download it:

wget http://ftp.math.purdue.edu/mirrors/opencsw.org/pkgutil-sparc.pkg

Tip: if you are running Solaris in VirtualBox then you can easily move it over using sshfs (see [my previous post](/blog/2009/9/14/solaris-10-in-virtualbox-quickstart) for details on using sshfs with a VirtualBox instance).

To install it and update the catalogue (I assume that everything is done as root from now on):

    export PATH=$PATH:/opt/csw/bin
    pkgadd -d pkgutil-sparc.pkg
    pkgutil -U

It's handy to have bash, wget, vim, and screen:

    pkgutil -i bash wget vim screen

A few things that we'll need for building gcc:

    pkgutil -i binutil autoconf automake gtar bzip2 gmake

I don't think that all of these exports are necessary apart from the path but my build worked with all of them:

    export LIBRARY_PATH="/usr/local/lib"
    export LIBRARY_PATH="/usr/local/lib"
    export LDFLAGS="-RLIBDIR"
    export LD_OPTIONS="-L/usr/local/lib -R/usr/local/lib"
    export PATH=$PATH:/usr/bin:/usr/sfw/bin:/opt/csw/bin:/usr/ccs/bin

I used the GNU assembler and the Sun linker:

    # /usr/sfw/bin/gas -v
    GNU assembler version 2.15 (i386-pc-solaris2.10) using BFD version 2.15
    # /usr/ccs/bin/ld -V
    ld: Software Generation Utilities - Solaris Link Editors: 5.10-1.493

Make sure that your ld and gas give same or later versions.

To compile gcc you will need gmp and mpfr:

    wget ftp://gcc.gnu.org/pub/gcc/infrastructure/gmp-4.2.4.tar.bz2
    gtar jxf gmp-4.2.4.tar.bz2
    cd gmp-4.2.4
    ./configure --prefix=/usr/local
    gmake
    gmake install
    gmake check
    cd ..

    wget ftp://gcc.gnu.org/pub/gcc/infrastructure/mpfr-2.4.1.tar.bz2
    gtar jxf mpfr-2.4.1.tar.bz2
    cd mpfr-2.4.1
    ./configure --prefix=/usr/local
    gmake
    gmake install
    cd ..

I did a full install of Solaris 10 so I ended up with this older gcc compiler:

    -bash-3.00$ /usr/sfw/bin/gcc -v
    Reading specs from /usr/sfw/lib/gcc/i386-pc-solaris2.10/3.4.3/specs
    Configured with: /builds/sfw10-gate/usr/src/cmd/gcc/gcc-3.4.3/configure --prefix=/usr/sfw --with-as=/usr/sfw/bin/gas --with-gnu-as --with-ld=/usr/ccs/bin/ld --without-gnu-ld --enable-languages=c,c++ --enable-shared
    Thread model: posix
    gcc version 3.4.3 (csl-sol210-3_4-branch+sol_rpath)

We will use that version of gcc to compile the newer one.

Warning: gcc is supposed to be compiled in a subdirectory of the main source tree. You can end up in trouble if you just run ./configure in the top level directory. Instead we work in the objdir directory. Pick a close mirror from [this list](http://gcc.gnu.org/mirrors.html) and then compile gcc (note that you need all of the environment variables (the export lines) from before). If it makes any difference, I did this in a bash shell, not sh (the default for root on Solaris).

    wget http://your-closest-mirror/.../gcc-4.2.4.tar.bz2
    gtar jxf gcc-4.2.4.tar.bz2
    cd gcc-4.2.4
    mkdir obj
    cd objdir
    ../configure --with-gnu-as --with-as=/usr/sfw/bin/gas 
    --without-gnu-ld --with-ld=/usr/ccs/bin/ld --enable-shared 
    --disable-nls --enable-languages=c,c++,objc,fortran 
    --disable-multilib
    gmake
    gmake install

Now make sure that /usr/local/bin is early enough in your path. For example:

    export PATH=$PATH:/usr/local/bin:/usr/sfw/bin:/opt/csw/bin:/usr/ccs/bin

And we have success:

    # which gcc
    /usr/local/bin/gcc

    # gcc -v
    Using built-in specs.
    Target: i386-pc-solaris2.10
    Configured with: ../configure --with-gnu-as --with-as=/usr/sfw/bin/gas --without-gnu-ld --with-ld=/usr/ccs/bin/ld --enable-shared --disable-nls --enable-languages=c,c++,objc,fortran --disable-multilib
    Thread model: posix
    gcc version 4.2.4

I had no luck with the gcc 4.4.x packages available via pkgutil on x86 basically due to a [a known problem](http://lists.opencsw.org/pipermail/bug-notifications/2009-August/001859.html).

I also tried to build gcc 4.4.1 from source and it failed like this:

    checking whether /export/home/carlo/gcc-4.4.1/build1/./gcc/xgcc
    -B/export/home/carlo/gcc-4.4.1/build1/./gcc/
    -B/usr/local//i386-pc-solaris2.10/bin/
    -B/usr/local//i386-pc-solaris2.10/lib/ -isystem
    /usr/local//i386-pc-solaris2.10/include -isystem
    /usr/local//i386-pc-solaris2.10/sys-include  -m64 supports -pedantic ...
    yes
    checking whether /export/home/carlo/gcc-4.4.1/build1/./gcc/xgcc
    -B/export/home/carlo/gcc-4.4.1/build1/./gcc/
    -B/usr/local//i386-pc-solaris2.10/bin/
    -B/usr/local//i386-pc-solaris2.10/lib/ -isystem
    /usr/local//i386-pc-solaris2.10/include -isystem
    /usr/local//i386-pc-solaris2.10/sys-include  -m64 and cc understand -c
    and -o together... yes
    checking for an ANSI C-conforming const... yes
    checking for inline... inline
    checking whether byte ordering is bigendian... unknown
    configure: error: unknown endianness
    presetting ac_cv_c_bigendian=no (or yes) will help
    gmake[1]: *** [configure-target-libiberty] Error 1
    gmake[1]: Leaving directory `/export/home/carlo/gcc-4.4.1/build1'
    gmake: *** [all] Error 2

Other people hit this same problem but I couldn't find a solution.

