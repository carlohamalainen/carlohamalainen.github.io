---
id: 821
title: Fix crackling audio in Skype on Debian Jessie
date: 2014-03-26T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2014/03/26/fix-crackling-audio-in-skype-on-debian-jessie/
permalink: /2014/03/26/fix-crackling-audio-in-skype-on-debian-jessie/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
On Debian Jessie my Skype audio had a horrible crackle and the microphone on my USB headset seemed to be at a very low level (barely audible on the test call). This fixed all of the audio problems: 

<pre>PULSE_LATENCY_MSEC=30 skype
</pre>

Credit: <https://bbs.archlinux.org/viewtopic.php?pid=1288881>.