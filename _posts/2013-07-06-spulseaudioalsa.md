---
id: 815
title: s/pulseaudio/alsa
date: 2013-07-06T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2013/07/06/spulseaudioalsa/
permalink: /2013/07/06/spulseaudioalsa/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
I can never work out how to get Skype to play nicely with pulseaudio on Debian/Ubuntu. So blow away pulseaudio and use alsa instead: 

<pre>sudo killall pulseaudio
sudo apt-get purge pulseaudio pulseaudio-utils gstreamer0.10-pulseaudio  paman pavumeter pavucontrol
rm ~/.pulse-cookie
rm -r ~/.pulse
sudo apt-get install alsa-base alsa-tools alsa-tools-gui alsa-utils alsa-oss alsamixergui libalsaplayer0
</pre>

One day I&#8217;ll work out how to get pavucontrol (?) to do its thing, or Jack, or whatever.