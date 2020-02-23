---
id: 753
title: 'Note to self: &#8220;cabal install libssh2&#8221; on Debian testing'
date: 2013-02-10T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2013/02/10/note-to-self-cabal-install-libssh2-on-debian-testing/
permalink: /2013/02/10/note-to-self-cabal-install-libssh2-on-debian-testing/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
Debian Squeeze doesn&#8217;t have a new enough libssh2 for the [LibSSH2](http://hackage.haskell.org/packages/archive/libssh2/latest/doc/html/Network-SSH-Client-LibSSH2.html) package on hackage. So I tried the libssh library and dev package from Debian testing: 

<pre>$ dpkg -l | grep -i libssh2
ii  libssh2-1                                       1.4.2-1.1                          SSH2 client-side library
ii  libssh2-1-dev                                   1.4.2-1.1                          SSH2 client-side library (development headers)
</pre>

However libssh2 failed to install using cabal: 

<pre>$ cabal install libssh2
Resolving dependencies...
Configuring libssh2-0.2.0.1...
cabal: The pkg-config package libssh2 version &gt;=1.2.8 is required but it could
not be found.
Failed to install libssh2-0.2.0.1
cabal: Error: some packages failed to install:
libssh2-0.2.0.1 failed during the configure step. The exception was:
ExitFailure 1
</pre>

We can help it along by setting PKG\_CONFIG\_PATH so that cabal finds libssh2, and also set the extra lib/include directories so that it finds libgcrypt: 

<pre>$ export PKG_CONFIG_PATH=/usr/lib/x86_64-linux-gnu/pkgconfig
$ cabal install libssh2 --extra-include-dirs=/usr/include --extra-lib-dirs=/lib/x86_64-linux-gnu
</pre>

Finally, compiling [ssh-client.hs](https://github.com/portnov/libssh2-hs/blob/master/libssh2/ssh-client.hs) blew up in a weird way: 

$ ghc &#8211;make ssh-client.hs  
[1 of 1] Compiling Main ( ssh-client.hs, ssh-client.o )

<pre>ssh-client.hs:22:18:
    Couldn't match expected type `BSL.ByteString'
                with actual type `bytestring-0.9.2.1:Data.ByteString.Lazy.Internal.ByteString'
    In the first argument of `BSL.putStr', namely `result'
    In a stmt of a 'do' block: BSL.putStr result
    In the expression:
      do { channelExecute ch command;
           result &lt;- readAllChannel ch;
           BSL.putStr result }
</pre>

Thanks to [this stackoverflow question](http://stackoverflow.com/questions/12576817/couldnt-match-expected-type-with-actual-type-error-when-using-codec-bmp) I found out that I had two versions of the bytestring library installed on my system: 

<pre>$ ghc-pkg list bytestring
/opt/sw/64bit/debian/ghc-7.4.2/lib/ghc-7.4.2/package.conf.d
   bytestring-0.9.2.1
/home/carlo/.ghc/x86_64-linux-7.4.2/package.conf.d
   bytestring-0.10.0.1
</pre>

and the answer was to hide the 0.10.x version:

<pre>$ ghc --make -hide-package bytestring-0.10.0.1 ssh-client.hs
</pre>

Example usage: 

<pre>$ ./ssh-client command carlo 192.168.1.70 22 uptime
 20:04:43 up 35 days, 22:14,  0 users,  load average: 0.00, 0.00, 0.00
</pre>