---
author: Carlo Hamalainen

date: "2014-03-26T00:00:00Z"
format: image
guid: http://carlo-hamalainen.net/2014/03/26/fix-crackling-audio-in-skype-on-debian-jessie/
title: Fix crackling audio in Skype on Debian Jessie
url: /2014/03/26/fix-crackling-audio-in-skype-on-debian-jessie/
---
On Debian Jessie my Skype audio had a horrible crackle and the microphone on my USB headset seemed to be at a very low level (barely audible on the test call). This fixed all of the audio problems: 

```
PULSE_LATENCY_MSEC=30 skype
```

Credit: <https://bbs.archlinux.org/viewtopic.php?pid=1288881>.
