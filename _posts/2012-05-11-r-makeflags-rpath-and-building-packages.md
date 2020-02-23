---
id: 722
title: R, MAKEFLAGS, rpath, and building packages
date: 2012-05-11T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2012/05/11/r-makeflags-rpath-and-building-packages/
permalink: /2012/05/11/r-makeflags-rpath-and-building-packages/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
On our HPC at work we need to build various libraries and packages from source, and install them to custom locations. Putting everything in /usr/local is not an option because of dependencies on particular versions of various libraries (and many of these packages are not available through the distro&#8217;s package manager). While building RODBC for a colleague I encountered a problem with library paths: 

<pre>library(RODBC)
Error in dyn.load(file, DLLpath = DLLpath, ...) :
   unable to load shared object
'/opt/RODBC/RODBC_1.3-5/RODBC/libs/RODBC.so':
   libodbc.so.1: cannot open shared object file: No such file or directory
Error: package/namespace load failed for ‘RODBC’
</pre>

The author of the package claimed that the solution is in the documentation, but I disagree. For the benefit of anyone who comes across this problem, here&#8217;s a log of how I debugged the problem.

First try to build with no options at all. Fails because it can&#8217;t find sql.h, as expected:

<pre>carlo@r500:/opt/src/RODBC&gt; R CMD INSTALL -l /opt/RODBC/RODBC_1.3-5 RODBC_1.3-5.tar.gz
* installing *source* package ‘RODBC’ ...
** package ‘RODBC’ successfully unpacked and MD5 sums checked
checking for gcc... gcc -std=gnu99
checking for C compiler default output file name... a.out
checking whether the C compiler works... yes
checking whether we are cross compiling... no
checking for suffix of executables...
checking for suffix of object files... o
checking whether we are using the GNU C compiler... yes
checking whether gcc -std=gnu99 accepts -g... yes
checking for gcc -std=gnu99 option to accept ANSI C... none needed
checking how to run the C preprocessor... gcc -std=gnu99 -E
checking for egrep... grep -E
checking for ANSI C header files... yes
checking for sys/types.h... yes
checking for sys/stat.h... yes
checking for stdlib.h... yes
checking for string.h... yes
checking for memory.h... yes
checking for strings.h... yes
checking for inttypes.h... yes
checking for stdint.h... yes
checking for unistd.h... yes
checking sql.h usability... no
checking sql.h presence... no
checking for sql.h... no
checking sqlext.h usability... no
checking sqlext.h presence... no
checking for sqlext.h... no
configure: error: "ODBC headers sql.h and sqlext.h not found"
ERROR: configuration failed for package ‘RODBC’
* removing ‘/opt/RODBC/RODBC_1.3-5/RODBC’
* restoring previous ‘/opt/RODBC/RODBC_1.3-5/RODBC’
</pre>

Now set location for ODBC library, using environment variables, as per the documentation:

<pre>export ODBC_INCLUDE=$ODBC_ROOT/include
export ODBC_LIBS=$ODBC_ROOT/lib

carlo@r500:/opt/src/RODBC&gt; R CMD INSTALL -l /opt/RODBC/RODBC_1.3-5 RODBC_1.3-5.tar.gz
* installing *source* package ‘RODBC’ ...
** package ‘RODBC’ successfully unpacked and MD5 sums checked
checking for gcc... gcc -std=gnu99
checking for C compiler default output file name... a.out
checking whether the C compiler works... yes
checking whether we are cross compiling... no
checking for suffix of executables...
checking for suffix of object files... o
checking whether we are using the GNU C compiler... yes
checking whether gcc -std=gnu99 accepts -g... yes
checking for gcc -std=gnu99 option to accept ANSI C... none needed
checking how to run the C preprocessor... gcc -std=gnu99 -E
checking for egrep... grep -E
checking for ANSI C header files... yes
checking for sys/types.h... yes
checking for sys/stat.h... yes
checking for stdlib.h... yes
checking for string.h... yes
checking for memory.h... yes
checking for strings.h... yes
checking for inttypes.h... yes
checking for stdint.h... yes
checking for unistd.h... yes
checking sql.h usability... yes
checking sql.h presence... yes
checking for sql.h... yes
checking sqlext.h usability... yes
checking sqlext.h presence... yes
checking for sqlext.h... yes
checking for library containing SQLTables... -lodbc
checking for SQLLEN... yes
checking for SQLULEN... yes
checking for long... yes
checking size of long... configure: error: cannot compute sizeof (long), 77
See `config.log' for more details.
ERROR: configuration failed for package ‘RODBC’
* removing ‘/opt/RODBC/RODBC_1.3-5/RODBC’
* restoring previous ‘/opt/RODBC/RODBC_1.3-5/RODBC’
</pre>

This error is an error itself; the problem is actually with linking against libodbc. The usual Unix way is to set LDFLAGS, so let&#8217;s try that:

<pre>export LDFLAGS="-L/opt/odbc/odbc-2.3.0/lib -Wl,-rpath /opt/odbc/odbc-2.3.0/lib"
R CMD INSTALL -l /opt/RODBC/RODBC_1.3-5 RODBC_1.3-5.tar.gz

* installing *source* package ‘RODBC’ ...
** package ‘RODBC’ successfully unpacked and MD5 sums checked
checking for gcc... gcc -std=gnu99
checking for C compiler default output file name... configure: error: C compiler cannot create executables
See `config.log' for more details.
ERROR: configuration failed for package ‘RODBC’
* removing ‘/opt/RODBC/RODBC_1.3-5/RODBC’
* restoring previous ‘/opt/RODBC/RODBC_1.3-5/RODBC’
</pre>

Perhaps not. After reading more, I found that &#8220;R CMD INSTALL&#8221; can make use of the MAKEFLAGS environment variable, in which [whitespaces have to be escaped](https://stat.ethz.ch/pipermail/r-help/2002-June/022393.html) (how odd). So let&#8217;s turn off LDFLAGS and try with that:

<pre>unset LDFLAGS
MAKEFLAGS='LDFLAGS=-L/opt/odbc/odbc-2.3.0/lib -Wl,-rpath /opt/odbc/odbc-2.3.0/lib' R CMD INSTALL -l /opt/RODBC/RODBC_1.3-5 RODBC_1.3-5.tar.gz

carlo@r500:/opt/src/RODBC&gt; MAKEFLAGS='LDFLAGS=-L/opt/odbc/odbc-2.3.0/lib -Wl,-rpath /opt/odbc/odbc-2.3.0/lib' R CMD INSTALL -l /opt/RODBC/RODBC_1.3-5 RODBC_1.3-5.tar.gz
* installing *source* package ‘RODBC’ ...
** package ‘RODBC’ successfully unpacked and MD5 sums checked
checking for gcc... gcc -std=gnu99
checking for C compiler default output file name... a.out
checking whether the C compiler works... yes
checking whether we are cross compiling... no
checking for suffix of executables...
checking for suffix of object files... o
checking whether we are using the GNU C compiler... yes
checking whether gcc -std=gnu99 accepts -g... yes
checking for gcc -std=gnu99 option to accept ANSI C... none needed
checking how to run the C preprocessor... gcc -std=gnu99 -E
checking for egrep... grep -E
checking for ANSI C header files... yes
checking for sys/types.h... yes
checking for sys/stat.h... yes
checking for stdlib.h... yes
checking for string.h... yes
checking for memory.h... yes
checking for strings.h... yes
checking for inttypes.h... yes
checking for stdint.h... yes
checking for unistd.h... yes
checking sql.h usability... yes
checking sql.h presence... yes
checking for sql.h... yes
checking sqlext.h usability... yes
checking sqlext.h presence... yes
checking for sqlext.h... yes
checking for library containing SQLTables... -lodbc
checking for SQLLEN... yes
checking for SQLULEN... yes
checking for long... yes
checking size of long... configure: error: cannot compute sizeof (long), 77
See `config.log' for more details.
ERROR: configuration failed for package ‘RODBC’
* removing ‘/opt/RODBC/RODBC_1.3-5/RODBC’
* restoring previous ‘/opt/RODBC/RODBC_1.3-5/RODBC’
</pre>

This still fails, so maybe we can try the LD\_LIBRARY\_PATH?

<pre>MAKEFLAGS='LD_LIBRARY_PATH=/opt/gcc/4.4.2/lib64:/opt/gcc/4.4.2/lib:/opt/odbc/odbc-2.3.0/lib LDFLAGS=-L/opt/odbc/odbc-2.3.0/lib -Wl,-rpath /opt/odbc/odbc-2.3.0/lib' R CMD INSTALL -l /opt/RODBC/RODBC_1.3-5 RODBC_1.3-5.tar.gz

* installing *source* package ‘RODBC’ ...
** package ‘RODBC’ successfully unpacked and MD5 sums checked
checking for gcc... gcc -std=gnu99
checking for C compiler default output file name... a.out
checking whether the C compiler works... yes
checking whether we are cross compiling... no
checking for suffix of executables...
checking for suffix of object files... o
checking whether we are using the GNU C compiler... yes
checking whether gcc -std=gnu99 accepts -g... yes
checking for gcc -std=gnu99 option to accept ANSI C... none needed
checking how to run the C preprocessor... gcc -std=gnu99 -E
checking for egrep... grep -E
checking for ANSI C header files... yes
checking for sys/types.h... yes
checking for sys/stat.h... yes
checking for stdlib.h... yes
checking for string.h... yes
checking for memory.h... yes
checking for strings.h... yes
checking for inttypes.h... yes
checking for stdint.h... yes
checking for unistd.h... yes
checking sql.h usability... yes
checking sql.h presence... yes
checking for sql.h... yes
checking sqlext.h usability... yes
checking sqlext.h presence... yes
checking for sqlext.h... yes
checking for library containing SQLTables... -lodbc
checking for SQLLEN... yes
checking for SQLULEN... yes
checking for long... yes
checking size of long... configure: error: cannot compute sizeof (long), 77
See `config.log' for more details.
ERROR: configuration failed for package ‘RODBC’
* removing ‘/opt/RODBC/RODBC_1.3-5/RODBC’
* restoring previous ‘/opt/RODBC/RODBC_1.3-5/RODBC’
</pre>

Nope, LD\_LIBRARY\_PATH is ignored when it&#8217;s inside MAKEFLAGS. Let&#8217;s be psychic and set it as a shell environment variable instead:

<pre>export LD_LIBRARY_PATH=/opt/gcc/4.4.2/lib64:/opt/gcc/4.4.2/lib:/opt/odbc/odbc-2.3.0/lib
MAKEFLAGS='LDFLAGS=-L/opt/odbc/odbc-2.3.0/lib -Wl,-rpath /opt/odbc/odbc-2.3.0/lib' R CMD INSTALL -l /opt/RODBC/RODBC_1.3-5 RODBC_1.3-5.tar.gz
* installing *source* package ‘RODBC’ ...
** package ‘RODBC’ successfully unpacked and MD5 sums checked
checking for gcc... gcc -std=gnu99
checking for C compiler default output file name... a.out
checking whether the C compiler works... yes
checking whether we are cross compiling... no
checking for suffix of executables...
checking for suffix of object files... o
checking whether we are using the GNU C compiler... yes
checking whether gcc -std=gnu99 accepts -g... yes
checking for gcc -std=gnu99 option to accept ANSI C... none needed
checking how to run the C preprocessor... gcc -std=gnu99 -E
checking for egrep... grep -E
checking for ANSI C header files... yes
checking for sys/types.h... yes
checking for sys/stat.h... yes
checking for stdlib.h... yes
checking for string.h... yes
checking for memory.h... yes
checking for strings.h... yes
checking for inttypes.h... yes
checking for stdint.h... yes
checking for unistd.h... yes
checking sql.h usability... yes
checking sql.h presence... yes
checking for sql.h... yes
checking sqlext.h usability... yes
checking sqlext.h presence... yes
checking for sqlext.h... yes
checking for library containing SQLTables... -lodbc
checking for SQLLEN... yes
checking for SQLULEN... yes
checking for long... yes
checking size of long... 8
configure: creating ./config.status
config.status: creating src/Makevars
config.status: creating src/config.h
** libs
gcc -std=gnu99 -I/opt/R/2.14.0/lib64/R/include -I. -I/opt/odbc/odbc-2.3.0/include -I/usr/local/include    -fpic  -g -O2 -c RODBC.c -o RODBC.o
gcc -std=gnu99 -shared -L/opt/odbc/odbc-2.3.0/lib -Wl,-rpath /opt/odbc/odbc-2.3.0/lib -o RODBC.so RODBC.o -lodbc -L/opt/odbc/odbc-2.3.0/lib -L/opt/R/2.14.0/lib64/R/lib -lR
installing to /opt/RODBC/RODBC_1.3-5/RODBC/libs
** R
** inst
** preparing package for lazy loading
** help
*** installing help indices
** building package indices ...
*** tangling vignette sources ...
   ‘RODBC.Rnw’
** testing if installed package can be loaded

* DONE (RODBC)
</pre>

Success. Note the &#8220;-Wl,-rpath&#8221; option which lets RODBC.so know where libodbc.so is, so that the end user running R doesn&#8217;t need to set any environment variables before loading RODBC. 

In summary:

<pre>export ODBC_INCLUDE=$ODBC_ROOT/include
export ODBC_LIBS=$ODBC_ROOT/lib
export LD_LIBRARY_PATH=/opt/gcc/4.4.2/lib64:/opt/gcc/4.4.2/lib:/opt/odbc/odbc-2.3.0/lib
MAKEFLAGS='LDFLAGS=-L/opt/odbc/odbc-2.3.0/lib -Wl,-rpath /opt/odbc/odbc-2.3.0/lib' R CMD INSTALL -l /opt/RODBC/RODBC_1.3-5 RODBC_1.3-5.tar.gz
</pre>

Further reading: <http://www.eyrie.org/~eagle/notes/rpath.html>

**Archived Comments**

Date: 2013-06-19 01:00:49 UTC

Author: Dhanesh Padmanabhan

Thanks. This was really useful. In my case, I also had to add an additional CPPFLAGS=-I. in MAKEFLAGS and that solved it.

Date: 2017-02-18 01:24:30.628406 UTC

Author: joshua

In case this helps anyone: the solution here did not work for me, but the one posted here did:

<http://r.789695.n4.nabble.com/Problem-installing-RODBC-td2016736.html> 

Just need this: yum install unixODBC-devel