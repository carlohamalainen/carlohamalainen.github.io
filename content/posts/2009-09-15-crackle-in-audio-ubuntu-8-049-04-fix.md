---
author: Carlo Hamalainen

date: "2009-09-15T00:00:00Z"
format: image
guid: http://carlo-hamalainen.net/2009/09/15/crackle-in-audio-ubuntu-8-049-04-fix/
id: 739
original_post_id:
- "16"
restapi_import_id:
- 596a05ef0330b
title: Crackle in audio (Ubuntu 8.04/9.04) fix
url: /2009/09/15/crackle-in-audio-ubuntu-8-049-04-fix/
---
The left audio channel on my work computer crackled badly. Ubuntu 8.04. Soundcard info from lspci:

    00:1b.0 Audio device: Intel Corporation 82801G (ICH7 Family) High Definition Audio Controller (rev 01)

Remove ``snd_hda_intel``:

    $ sudo modprobe -r snd_hda_intel
    FATAL: Module snd_hda_intel is in use.

    $ sudo lsof /dev/snd/* | grep mixer
    COMMAND    PID  USER   FD   TYPE DEVICE SIZE  NODE NAME
    mixer_app 9593 carlo   20u   CHR  116,0      11357 /dev/snd/controlC0

    $ kill -9 (pid of mixer_app)

The fix: load with ``position_fix`` and ``model`` parameters.

    $ sudo modprobe snd-hda-intel position_fix=1 model=3stack

Then use alsamixer to turn up and unmute main/pcm/etc.
