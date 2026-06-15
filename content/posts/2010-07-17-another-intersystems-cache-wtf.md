---
author: Carlo Hamalainen

date: "2010-07-17T00:00:00Z"
format: image
title: Another Intersystems Caché WTF
url: /2010/07/17/another-intersystems-cache-wtf/
---
I stumbled across this remark in some documentation for [Intersystems Caché ObjectScript](http://vista.intersystems.com/csp/docbook/DocBook.UI.Page.cls?KEY=TCOS_Integrity).

> You use the Lock command to prevent multiple processes from updating the same record at the same time. But it only works by convention: all the code throughout an application that updates a given global must try to Lock the record that is to be updated, and unLock it when finished. If one routine uses Lock, but another doesn't, nothing prevents the second routine from updating the record while the first routine has it locked.

