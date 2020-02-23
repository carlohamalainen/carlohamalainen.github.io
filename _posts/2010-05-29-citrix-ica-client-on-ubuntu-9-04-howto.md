---
id: 714
title: Citrix ICA client on Ubuntu 9.04 howto
date: 2010-05-29T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2010/05/29/citrix-ica-client-on-ubuntu-9-04-howto/
permalink: /2010/05/29/citrix-ica-client-on-ubuntu-9-04-howto/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
Notes on installing the Citrix ICA client on Ubuntu 9.04.

Download the 32bit deb file from [here](http://www.citrix.com/English/ss/downloads/details.asp?downloadId=3323&productId=186), in particular [this file](http://download.citrix.com.edgesuite.net/akdlm/4898/icaclient_11.100_i386.patched.deb?__gda__=1275095537_600cd6f90e8f6a5bb239625d93d0119f&__dlmgda__=1275181637_40f74f639e072102fb80353b19e208d2&fileExt=.deb).

For some reason I ended up with a non .deb file:

<pre>$ mv unconfirmed 17720.download citrix.deb
</pre>

Install the deb:

<pre>$ sudo dpkg -i citrix.deb
</pre>

Now install libXm and cheat with the version number:

<pre>$ sudo aptitude install libmotif3
$ sudo ln -s `ls /usr/lib/libXm.so*|sort|tail -1` /usr/lib/libXm.so.4
</pre>

Now try the Citrix receiver. There should be no error message.

<pre>/usr/lib/ICAClient/wfcmgr
</pre>

I had no luck with Chrome but Firefox happily launched the Citrix client when I logged into my Active Directory desktop at work.

Also note that the default key to step out of the Citrix desktop is Ctrl+F2 (then you can alt-tab elsewhere) whereas on a Windows client you use Shift+F2 to go out of fullscreen and then alt-tab somewhere.

Thanks to the [Ubuntu community wiki](https://help.ubuntu.com/community/CitrixICAClientHowTo) for all the key details.