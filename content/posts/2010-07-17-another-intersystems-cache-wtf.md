---
author: Carlo Hamalainen

date: "2010-07-17T00:00:00Z"
format: image
guid: http://carlo-hamalainen.net/2010/07/17/another-intersystems-cache-wtf/
id: 725
original_post_id:
- "16"
restapi_import_id:
- 596a05ef0330b
title: Another Intersystems Caché WTF
url: /2010/07/17/another-intersystems-cache-wtf/
---
I stumbled across this remark in some documentation for [Intersystems Caché ObjectScript](http://vista.intersystems.com/csp/docbook/DocBook.UI.Page.cls?KEY=TCOS_Integrity).

> You use the Lock command to prevent multiple processes from updating the same record at the same time. But it only works by convention: all the code throughout an application that updates a given global must try to Lock the record that is to be updated, and unLock it when finished. If one routine uses Lock, but another doesn't, nothing prevents the second routine from updating the record while the first routine has it locked.

**Archived Comments**

Date: 2010-11-19 17:21:07 UTC

Author: Joe Bayus

Yes, it's true, and always has been for M installations. One advantage is that it prevents someone from locking up entire nodes or huge sections of the data (locking a node locks all the entries "below" it) -- it also requires a well-organized and coordinated system design to work properly. Always something worth encouraging, don't you think?  
Joe B

Date: 2010-11-20 00:13:40 UTC

Author: carlo

If locks aren't quite locks, what happens to transactions?

Date: 2010-12-17 20:34:01 UTC

Author: Jim Volstad

A record (?) is locked and other jobs can still update it? OMG!