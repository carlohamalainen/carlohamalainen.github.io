---
author: Carlo Hamalainen

date: "2014-03-16T00:00:00Z"
format: image
guid: http://carlo-hamalainen.net/2014/03/16/note-to-self-ffmpeg-low-quality/
title: 'Note to self: ffmpeg low quality.'
url: /2014/03/16/note-to-self-ffmpeg-low-quality/
---
Sometimes high quality video files (e.g. mkv) don't play on my HP Mini Netbook, so here is an ffmpeg command to rescale the video to lower quality: 

```
opts='-qscale 5 -r 25 -ar 44100 -ab 96 -s 500x374'

ffmpeg -y -i infile.mkv ${opts} outfile.mp4
```
