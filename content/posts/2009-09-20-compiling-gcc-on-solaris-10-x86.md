---
author: Carlo Hamalainen

date: "2009-09-20T00:00:00Z"
format: image
guid: http://carlo-hamalainen.net/2009/09/20/compiling-gcc-on-solaris-10-x86/
id: 765
original_post_id:
- "16"
restapi_import_id:
- 596a05ef0330b
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

**Archived Comments**

Date: 2013-11-05 13:56:41.210158 UTC

Author: Stephen L

Doesnt work for me. The compile of gmp fails straight away with

    libtool: link: only absolute run-paths are allowed

Date: 2014-07-14 11:44:54.794208 UTC

Author: Aejaz

don't use this option ``export LDFLAGS="-RLIBDIR"``, gmp will install, if you have already set it use ``export LDFLAGS=``

Date: 2014-07-24 14:58:44.657682 UTC

Author: UX-admin

Doesnt work for me. The compile of gmp fails straight away with libtool: link: only absolute run-paths are allowed. 

That's because of the dumbass libtool (or rather, the author of  
it): libtool has no notion of the special $ORIGIN ld keyword. There  
are multiple possible fixes for that: 

 * never use LDFLAGS; O='$$O'; export O; ORIGIN='$ORIGIN'; export ORIGIN; LD_OPTIONS='$ORIGIN:$ORIGIN/../../lib:$ORIGIN/../lib:/opt/gcc/lib'; export LD_OPTIONS 
 * patch the idiotic libtool by applying the following patch, offsets will vary depending on the version of libtool, but the fix is always the same: 

```
--- libtool.orig
+++ libtool
@@ -5038,13 +5038,6 @@
          continue
          ;;
        rpath | xrpath)
-         # We need an absolute path.
-         case $arg in
-         [\/]* | [A-Za-z]:[\/]*) ;;
-         *)
-           func_fatal_error "only absolute run-paths are allowed"
-           ;;
-         esac
          if test "$prev" = rpath; then
            case "$rpath " in
            *" $arg "*) ;;
@@ -5351,13 +5344,6 @@
       -R*)
        func_stripname '-R' '' "$arg"
        dir=$func_stripname_result
-       # We need an absolute path.
-       case $dir in
-       [\/]* | [A-Za-z]:[\/]*) ;;
-       *)
-         func_fatal_error "only absolute run-paths are allowed"
-         ;;
-       esac
        case "$xrpath " in
        *" $dir "*) ;;
        *) xrpath="$xrpath $dir" ;;
```

apply the patch with: 

    /usr/sfw/bin/gpatch -uNp0; libtool.patch

``LD_OPTIONS`` is a very powerful variable, because the link editor will inject into his option stream whatever is in it right before the final link-edit stage. It's basically a backdoor override into the linker, and as such, will work with any compiler in existence on Solaris which uses ld. ``LD_OPTIONS`` enables compiling most software with Sun Studio compilers, which produce much better / faster / more optimized code than GCC, but this trick works with GCC just as well. 

With this patch and ``LD_OPTIONS`` or a combination of both, it is possible to build 99% of the freeware open source software on Solaris in record times without any hassles. Whatever won't build with Sun Studio will build with GCC with a combination of these two applied, unless it's kernel code for a non SunOS kernel. 

To wit: 

Using built-in specs. 

    COLLECT_GCC=gcc
    COLLECT_LTO_WRAPPER=/opt/gcc/lib/gcc/sparc-sun-solaris2.10/4.9.1/lto-wrapper
    Target: sparc-sun-solaris2.10
    Configured with: ../gcc-4.9.1/./configure --prefix=/opt/gcc --libexecdir=/opt/gcc/lib --libdir=/opt/gcc/lib --with-mpc=/opt/gcc --with-mpfr=/opt/gcc --with-gmp=/opt/gcc --with-local-prefix=/opt/gcc --with-as=/usr/ccs/bin/as --without-gnu-as --with-ld=/usr/ccs/bin/ld --without-gnu-ld --enable-languages=c,c++,fortran --enable-shared --disable-bootstrap --with-libiconv-prefix=/opt/gcc --with-system-zlib --disable-nls --enable-threads=posix : (reconfigured) ../gcc-4.9.1/./configure --prefix=/opt/gcc --libexecdir=/opt/gcc/lib --libdir=/opt/gcc/lib --with-mpc=/opt/gcc --with-mpfr=/opt/gcc --with-gmp=/opt/gcc --with-local-prefix=/opt/gcc --with-as=/usr/ccs/bin/as --without-gnu-as --with-ld=/usr/ccs/bin/ld --without-gnu-ld --enable-languages=c,c++,fortran --enable-shared --disable-bootstrap --with-libiconv-prefix=/opt/gcc --with-system-zlib --disable-nls --enable-threads=posix
    Thread model: posix
    gcc version 4.9.1 (GCC)

Post scriptum: 

on Solaris, always build GCC to use /usr/ccs/bin/as and  
/usr/ccs/bin/ld, irrespective of platform, for performance and  
because GNU tools have too many bugs, as well as are missing platform  
support. One cannot fully utilize the full platform support on Solaris  
with GNU assembler and linker. That is even stated in the official  
GNU compiler collection documentation.

Date: 2014-07-24 15:04:33.148973 UTC

Author: UX-admin

Whoops, ``LD_OPTIONS`` should be:

    LD_OPTIONS='-B direct -z ignore -R$ORIGIN:$ORIGIN/../../lib:$ORIGIN/../lib:/opt/gcc/lib'; export LD_OPTIONS
