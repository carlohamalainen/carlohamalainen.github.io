---
id: 755
title: MetaPost Examples
date: 2007-12-11T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2007/12/11/metapost-examples/
permalink: /2007/12/11/metapost-examples/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
**2008-10-19**: I use [PyX](http://pyx.sourceforge.net/) instead of Metapost. It's based on Python so the syntax is clear and straightforward, and is easily used from Sage (install with "sage -python setup.py install" from the PyX source directory).

### Introduction

My favourite package for drawing diagrams is [MetaPost](http://en.wikipedia.org/wiki/MetaPost). Here's an example, taken from my PhD thesis:

<img class="displayed" src="/stuff/myfiles/metapostrules.png?w=1100&ssl=1" alt="Metapost diagram" data-recalc-dims="1" /> 

The best page for examples is the one by [(La)TeX Navigator](http://tex.loria.fr/prod-graph/zoonekynd/metapost/metapost.html).

### Compiling with LaTeX fonts

Normally MetaPost uses TeX to compile but it is nicer to have access to LaTeX fonts/symbols/etc. To do this put the following at the top of your MetaPost file: 

    verbatimtex
    documentclass[12pt]{article}

    usepackage{amsmath}
    usepackage{amssymb}
    usepackage{amsthm}

    begin{document}
    etex

Then set the TEX environment variable and compile. For example, in  
Bash, you could do:

    $ export TEX=latex && mpost case1.mp

### My examples

Here are all the MetaPost diagrams from my PhD thesis. There are a few duplicates and some dodgy code, I make no claim of being an expert in MetaPost. 

[metapostexamples.pdf](/stuff/myfiles/metapost/metapostexamples/metapostexamples.pdf) 

[Browse files](/stuff/myfiles/metapost/metapostexamples/) 

[All files in a compressed tar archive](/stuff/myfiles/metapost/metapostexamples.tgz) 

### Metapost to PDF

A few of my Metapost files use the following TEX function (copied from [here](http://tex.loria.fr/prod-graph/zoonekynd/metapost/macros.mp)):

    vardef TEX primary s =
       write "verbatimtex"                   to "mptextmp.mp";
       write "documentclass[12pt]{article}"  to "mptextmp.mp";
       write "usepackage[T1]{fontenc}"       to "mptextmp.mp";
       write "usepackage{amsmath,amssymb}"   to "mptextmp.mp";
       write "begin{document}"               to "mptextmp.mp";
       write "etex"                          to "mptextmp.mp";
       write "btex "&s&" etex"               to "mptextmp.mp";
       write EOF                             to "mptextmp.mp";
       scantokens "input mptextmp"
    enddef;

The command ``mptopdf`` seems to have trouble with the temporary file ``mptextmp.mp``. In particular I got this error when doing ``mptopdf bug.mp``:

    This is MetaPost, Version 0.901 (Web2C 7.5.5)
    (/usr/share/texmf-texlive/web2c/natural.tcx)
    (bug.mp (mptextmp.mp
    >> mptextmp.mp
    >> mptextmp.mpx
    ! Unable to make mpx file.
    l.5 btex
             1 etex
    Transcript written on bug.log.
     error in metapost run : bug.mp:5

            total run time : 0 seconds

    MPtoPDF 1.3 : error while processing mp file

On the other hand, this seems to work:

    mpost bug.mp
    mptopdf bug.1

I'm not sure why this is the case -- is this a bug or just undocumented behaviour?

###  Other stuff

<a href="http://wiki.contextgarden.net/MetaFun">MetaFun</a> looks good. It's also worth checking out what other peope have <a href="http://del.icio.us/search/?fr=del_icio_us&p=metapost&type=all">tagged as metapost</a> on <a href="http://del.icio.us">del.icio.us</a>.
