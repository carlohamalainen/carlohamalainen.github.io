---
author: Carlo Hamalainen
categories:
date: "2007-12-11T00:00:00Z"
format: image
guid: http://carlo-hamalainen.net/2007/12/11/metapost-examples/
id: 755
original_post_id:
- "16"
restapi_import_id:
- 596a05ef0330b
title: MetaPost Examples
url: /2007/12/11/metapost-examples/
---

**2008-10-19**: I use [PyX](https://pyx-project.org/) instead of Metapost. It's based on Python so the syntax is clear and straightforward, and is easily used from Sage (install with ``sage -python setup.py install`` from the PyX source directory).

### Introduction

My favourite package for drawing diagrams is [MetaPost](http://en.wikipedia.org/wiki/MetaPost). Here's an example, taken from my PhD thesis:

{{< figure src="/stuff/myfiles/metapostrules.png" >}}

The best page for examples is the one by [(La)TeX Navigator](http://tex.loria.fr/prod-graph/zoonekynd/metapost/metapost.html).

### Compiling with LaTeX fonts

Normally MetaPost uses TeX to compile but it is nicer to have access to LaTeX fonts/symbols/etc. To do this put the following at the top of your MetaPost file: 

```latex
verbatimtex
\documentclass[12pt]{article}

\usepackage{amsmath}
\usepackage{amssymb}
\usepackage{amsthm}

\begin{document}
etex
```

Then set the TEX environment variable and compile. For example, in  
Bash, you could do:

    $ export TEX=latex && mpost case1.mp

### My examples

Here are all the MetaPost diagrams from my PhD thesis. There are a few duplicates and some dodgy code, I make no claim of being an expert in MetaPost. 

[metapostexamples.pdf](/stuff/myfiles/metapost/metapostexamples/metapostexamples.pdf) 

[All files in a compressed tar archive](/stuff/myfiles/metapost/metapostexamples.tgz) 

### Metapost to PDF

A few of my Metapost files use the following TEX function (copied from [here](http://tex.loria.fr/prod-graph/zoonekynd/metapost/macros.mp)):

```
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
```

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

```
mpost bug.mp
mptopdf bug.1
```

I'm not sure why this is the case -- is this a bug or just undocumented behaviour?

###  Other stuff

[MetaFun](http://wiki.contextgarden.net/MetaFun) looks good. 