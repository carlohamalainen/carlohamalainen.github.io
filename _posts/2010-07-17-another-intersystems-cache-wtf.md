---
id: 725
title: Another Intersystems Caché WTF
date: 2010-07-17T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2010/07/17/another-intersystems-cache-wtf/
permalink: /2010/07/17/another-intersystems-cache-wtf/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
I stumbled across this remark in some documentation for [Intersystems Caché ObjectScript](http://vista.intersystems.com/csp/docbook/DocBook.UI.Page.cls?KEY=TCOS_Integrity).

> You use the Lock command to prevent multiple processes from updating the same record at the same time. But it only works by convention: all the code throughout an application that updates a given global must try to Lock the record that is to be updated, and unLock it when finished. If one routine uses Lock, but another doesn&#8217;t, nothing prevents the second routine from updating the record while the first routine has it locked.

**Archived Comments**

Date: 2010-11-19 17:21:07 UTC

Author: Joe Bayus

Yes, it&#8217;s true, and always has been for M installations. One advantage is that it prevents someone from locking up entire nodes or huge sections of the data (locking a node locks all the entries &#8220;below&#8221; it) &#8211; it also requires a well-organized and coordinated system design to work properly. Always something worth encouraging, don&#8217;t you think?  
Joe B

Date: 2010-11-20 00:13:40 UTC

Author: carlo

If locks aren&#8217;t quite locks, what happens to transactions?

Date: 2010-12-17 20:34:01 UTC

Author: Jim Volstad

A record (?) is locked and other jobs can still update it? OMG!