---
id: 814
title: Disable X1 Carbon touchscreen
date: 2013-09-25T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2013/09/25/disable-x1-carbon-touchscreen/
permalink: /2013/09/25/disable-x1-carbon-touchscreen/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
My Lenovo X1 Carbon has a touchscreen but I rarely use it. Sometimes on waking from suspend it generates spurious clicks that do things on my desktop like creating new folders. To disable the touchscreen completely, edit /etc/X11/xorg.conf.d/disable-touchscreen.conf as follows:

```
Section "InputClass"
    Identifier      "Annoying Touch Screen"
    Driver          "egalax"
    MatchProduct    "eGalax Inc. eGalaxTouch EXC7903-66v03_T1"

    Option          "DeviceEnabled" "0"
EndSection
```

To work out the product name, use xinput: 

```
$ xinput
⎡ Virtual core pointer                      id=2    [master pointer  (3)]
⎜   ↳ Virtual core XTEST pointer                id=4    [slave  pointer  (2)]
⎜   ↳ eGalax Inc. eGalaxTouch EXC7903-66v03_T1  id=9    [slave  pointer  (2)]
⎜   ↳ SynPS/2 Synaptics TouchPad                id=12   [slave  pointer  (2)]
⎜   ↳ TPPS/2 IBM TrackPoint                     id=13   [slave  pointer  (2)]
⎣ Virtual core keyboard                     id=3    [master keyboard (2)]
    ↳ Virtual core XTEST keyboard               id=5    [slave  keyboard (3)]
    ↳ Power Button                              id=6    [slave  keyboard (3)]
    ↳ Video Bus                                 id=7    [slave  keyboard (3)]
    ↳ Sleep Button                              id=8    [slave  keyboard (3)]
    ↳ Integrated Camera                         id=10   [slave  keyboard (3)]
    ↳ AT Translated Set 2 keyboard              id=11   [slave  keyboard (3)]
    ↳ ThinkPad Extra Buttons                    id=14   [slave  keyboard (3)]
```

If you want to disable the touchscreen temporarily, instead of the Xorg tweak try this command: 

    xinput --set-prop 'eGalax Inc. eGalaxTouch EXC7903-66v03_T1' 'Device Enabled' 0

Thanks to Ian on the [CLUG mailing list](https://lists.samba.org/archive/linux/2013-September/032506.html) for help with the Xorg configuration.
