---
id: 752
title: ssh socks proxy
date: 2008-02-20T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2008/02/20/ssh-socks-proxy/
permalink: /2008/02/20/ssh-socks-proxy/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
I recently discovered that OpenSSH has a built in SOCKS proxy. This is really handy for browsing the net from another computer. In my situation I need access to sites like [MathSciNet](http://www.ams.org/mathscinet/) and [JSTOR](http://www.jstor.org/). My university has a subscription so I need to appear to be browsing the net from a certain IP address.

From my home computer I can do:

    $ ssh -ND 9999 carlo@my-uni-computer

(after logging in no shell is given due to the ``-N`` option)

Then in Firefox I set the proxy to use SOCKS host localhost at port 9999. That's it. Firefox plugins like [FoxyProxy](https://addons.mozilla.org/en-US/firefox/addon/2464) or [SwitchProxy](https://addons.mozilla.org/en-US/firefox/addon/125) make switching the proxy setting quick and easy.

You can also use this to browse securely from a public wifi access point, avoid stupid web proxies, etc.
