---
author: Carlo Hamalainen

date: "2013-07-06T00:00:00Z"
format: image
guid: http://carlo-hamalainen.net/2013/07/06/spulseaudioalsa/
title: s/pulseaudio/alsa
url: /2013/07/06/spulseaudioalsa/
---
I can never work out how to get Skype to play nicely with pulseaudio on Debian/Ubuntu. So blow away pulseaudio and use alsa instead: 

```
sudo killall pulseaudio
sudo apt-get purge pulseaudio pulseaudio-utils gstreamer0.10-pulseaudio  paman pavumeter pavucontrol
rm ~/.pulse-cookie
rm -r ~/.pulse
sudo apt-get install alsa-base alsa-tools alsa-tools-gui alsa-utils alsa-oss alsamixergui libalsaplayer0
```

One day I'll work out how to get pavucontrol (?) to do its thing, or Jack, or whatever.
