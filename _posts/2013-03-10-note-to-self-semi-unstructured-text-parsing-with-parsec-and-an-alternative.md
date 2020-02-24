---
id: 808
title: 'note to self: semi-unstructured text parsing with Parsec (and an alternative)'
date: 2013-03-10T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2013/03/10/note-to-self-semi-unstructured-text-parsing-with-parsec-and-an-alternative/
permalink: /2013/03/10/note-to-self-semi-unstructured-text-parsing-with-parsec-and-an-alternative/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
Managed to answer someone's [question](http://www.haskell.org/pipermail/haskell-cafe/2013-March/106776.html) on haskell-cafe about parsing semi-structured text with Parsec: 

<https://gist.github.com/carlohamalainen/5087207.js>

S. Doaitse Swierstra [pointed out](http://www.haskell.org/pipermail/haskell-cafe/2013-March/106834.html) that the [Data.List.Grouping](http://hackage.haskell.org/packages/archive/list-grouping/0.1.1/doc/html/Data-List-Grouping.html) package may be more appropriate here.