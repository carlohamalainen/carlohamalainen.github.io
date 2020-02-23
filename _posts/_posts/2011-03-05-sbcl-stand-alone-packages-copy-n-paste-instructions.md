---
id: 825
title: 'SBCL stand alone + packages: copy &#8216;n&#8217; paste instructions'
date: 2011-03-05T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2011/03/05/sbcl-stand-alone-packages-copy-n-paste-instructions/
permalink: /2011/03/05/sbcl-stand-alone-packages-copy-n-paste-instructions/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
Here are copy &#8216;n&#8217; paste instructions for compiling and installing [SBCL](http://www.sbcl.org/), and installing packages manually. To compile SBCL we need an earlier SBCL binary; otherwise you may need to bootstrap using CLISP or CMUCL or some other Lisp variant. The SBCL installation guide has full details. 

<pre>tar jxf sbcl-1.0.46-source.tar.bz2
cd sbcl-1.0.46
mkdir /opt/sbcl-1.0.46
sh make.sh --prefix=/opt/sbcl-1.0.46      # different if you don't have an earlier SBCL binary available
INSTALL_ROOT=/opt/sbcl-1.0.46 sh install.sh
</pre>

Set the PATH and SBCL home directory in your ~/.bashrc:

<pre>export PATH=$PATH:/opt/sbcl-1.0.46/bin
export SBCL_HOME=/opt/sbcl-1.0.46/lib/sbcl
</pre>

Install some custom packages in /opt/lisp. For example, the [CFFI](http://common-lisp.net/project/cffi) package requires [babel](http://common-lisp.net/project/babel/), [Alexandria](http://common-lisp.net/project/alexandria/), and [trivial-features](http://www.cliki.net/trivial-features). Untar each package in /opt/lisp and then create (absolute) links to each package&#8217;s asd file:

<pre>cd /opt/lisp
ln -s /opt/lisp/alexandria/alexandria.asd .
ln -s /opt/lisp/babel_0.3.0/babel.asd .
ln -s /opt/lisp/cffi_0.10.6/cffi.asd .
ln -s /opt/lisp/trivial-features_0.6/trivial-features.asd .
</pre>

Point SBCL to this package directory by adding these lines to ~/.sbclrc:

<pre>(require 'asdf)
(pushnew #P"/opt/lisp/" asdf:*central-registry* :test #'equal)
(push #P"/opt/lisp/" asdf:*central-registry*)
</pre>

Now test that we can load the CFFI package:

<pre>carlo@foo:~&gt; sbcl
This is SBCL 1.0.46, an implementation of ANSI Common Lisp.
More information about SBCL is available at .

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
* (asdf:operate 'asdf:load-op 'cffi)

; loading system definition from
; /opt/lisp/cffi.asd into #
; registering # as CFFI
; loading system definition from
; /opt/lisp/babel.asd into #
; registering # as BABEL
; loading system definition from
; /opt/lisp/alexandria.asd into
; #
; registering # as ALEXANDRIA
; loading system definition from
; /opt/lisp/trivial-features.asd into
; #
; registering # as TRIVIAL-FEATURES

(lots more output snipped)
</pre>