---
id: 816
title: cu instead of minicom
date: 2010-06-06T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2010/06/06/cu-instead-of-minicom/
permalink: /2010/06/06/cu-instead-of-minicom/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
To connect to a SheevaPlug serial USB console, plug it in, and then use this one liner:

<pre>cu -s 115200 -l /dev/ttyUSB0
</pre>

Quicker than messing around with minicom and setting the device in a profile, etc. Thanks to [Wilfred on the computingplugs wiki](http://computingplugs.com/index.php/Connecting_to_the_serial_console).