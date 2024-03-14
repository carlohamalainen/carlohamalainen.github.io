My original message:

    Date: Thu, 22 Mar 2012 13:57:06 +1000
    From: Carlo Hamalainen <carlo@carlo-hamalainen.net>
    To: ripley@stats.ox.ac.uk
    Subject: link time options for RODBC

    Hi Brian,

    I'm building your RODBC package on a system with the ODBC library
    installed in a custom location.

    This works for the install:

    export LD_LIBRARY_PATH=3D$LD_LIBRARY_PATH:$ODBC_ROOT/lib
    export ODBC_INCLUDE=3D$ODBC_ROOT/include
    export ODBC_LIBS=3D$ODBC_ROOT/lib

    R CMD INSTALL -l /opt/sw/fw/qce/RODBC/RODBC_1.3-5 RODBC_1.3-5.tar.gz

    But on loading RODBC it can't find a shared library:

    > library(RODBC)
    Error in dyn.load(file, DLLpath =3D DLLpath, ...) :=20
      unable to load shared object
    '/opt/sw/fw/qce/RODBC/RODBC_1.3-5/RODBC/libs/RODBC.so':
      libodbc.so.1: cannot open shared object file: No such file or
    directory
    Error: package/namespace load failed for =E2=80=98RODBC=E2=80=99

    A workaround is to manually load the library first:

    > dyn.load("/opt/sw/fw/rsc/odbc/odbc-2.3.0/lib/libodbc.so.1")
    > library(RODBC) # (this works)



    My question: can I pass link-time options to the configure script? I
    tried setting LDFLAGS but it didn't change the actual gcc command line
    used by R to compile the RODBC library. Ideally I would pass an rpath
    argument to the linker and then the RODBC library would know where to
    get libodbc from.

    Cheers,

    --
    Carlo Hamalainen
    http://carlo-hamalainen.net


What could possibly go wrong?


    Date: Thu, 22 Mar 2012 06:58:48 +0000
    From: Prof Brian Ripley <ripley@stats.ox.ac.uk>
    To: Carlo Hamalainen <carlo@carlo-hamalainen.net>
    Subject: Re: link time options for RODBC

    This is all documented.  Please do you own homework.

    On 22/03/2012 03:57, Carlo Hamalainen wrote:
    > Hi Brian,

    Do I know you?

    > I'm building your RODBC package on a system with the ODBC library
    > installed in a custom location.
    >
    > This works for the install:
    >
    > export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$ODBC_ROOT/lib
    > export ODBC_INCLUDE=$ODBC_ROOT/include
    > export ODBC_LIBS=$ODBC_ROOT/lib
    >
    > R CMD INSTALL -l /opt/sw/fw/qce/RODBC/RODBC_1.3-5 RODBC_1.3-5.tar.gz
    >
    > But on loading RODBC it can't find a shared library:
    >
    >> library(RODBC)
    > Error in dyn.load(file, DLLpath = DLLpath, ...) :
    >    unable to load shared object
    > '/opt/sw/fw/qce/RODBC/RODBC_1.3-5/RODBC/libs/RODBC.so':
    >    libodbc.so.1: cannot open shared object file: No such file or
    > directory
    > Error: package/namespace load failed for ‘RODBC’
    >
    > A workaround is to manually load the library first:
    >
    >> dyn.load("/opt/sw/fw/rsc/odbc/odbc-2.3.0/lib/libodbc.so.1")
    >> library(RODBC) # (this works)
    >
    >
    >
    > My question: can I pass link-time options to the configure script? I
    > tried setting LDFLAGS but it didn't change the actual gcc command line
    > used by R to compile the RODBC library. Ideally I would pass an rpath
    > argument to the linker and then the RODBC library would know where to
    > get libodbc from.
    >
    > Cheers,
    >

    Sending unsigned emails is rude, whoever you are.

    -- 
    Brian D. Ripley,                  ripley@stats.ox.ac.uk
    Professor of Applied Statistics,  http://www.stats.ox.ac.uk/~ripley/
    University of Oxford,             Tel:  +44 1865 272861 (self)
    1 South Parks Road,                     +44 1865 272866 (PA)
    Oxford OX1 3TG, UK                Fax:  +44 1865 272595


![fry](futuramafry.jpg)
