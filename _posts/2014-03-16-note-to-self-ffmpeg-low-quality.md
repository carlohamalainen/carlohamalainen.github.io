---
id: 729
title: 'Note to self: ffmpeg low quality.'
date: 2014-03-16T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2014/03/16/note-to-self-ffmpeg-low-quality/
permalink: /2014/03/16/note-to-self-ffmpeg-low-quality/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
Sometimes high quality video files (e.g. mkv) don't play on my HP Mini Netbook, so here is an ffmpeg command to rescale the video to lower quality: 

<pre>opts='-qscale 5 -r 25 -ar 44100 -ab 96 -s 500x374'

ffmpeg -y -i infile.mkv ${opts} outfile.mp4
</pre>