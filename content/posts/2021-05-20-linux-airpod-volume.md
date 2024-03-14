---
author: Carlo Hamalainen
date: "2021-05-20T00:00:00Z"
title: Set AirPod volume on Linux
url: /2021/05/20/airpods
---

I have no idea why, but a week ago my AirPods suddenly became far too quiet on Linux (Debian buster). 
Changing volumes in pavucontrol didn't help, nor did tweaks that people suggested like setting
``--noplugin=avrcp`` nor ``--plugin=a2dp``.

The issue seems to be that Bluetooth headphones have an internal volume level. When connected to an iPhone this 
volume is set appropriately but something goes wrong on Linux.

I found [this post](https://unix.stackexchange.com/questions/437468/bluetooth-headset-volume-too-low-only-in-arch/562381#562381) which suggested forcing ``return TRUE`` in ``volume_exists`` in the bluez source code,
which then lets you change the AirPods volume via a dbus command.

On Debian/Ubuntu, building bluez is fairly easy. I followed [this guide](https://www.blog.willandnora.com/2017/08/06/build-bluez-5-46-yourself-for-your-debian-jessie/).

Install things:

    sudo apt install ccache
    sudo apt install build-essential devscripts lintian diffutils patch patchutils
    apt-get source bluez
    sudo apt-get build-dep bluez

    cd bluez-5.50

Edit ``profiles/audio/transport.c``:

    630 static gboolean volume_exists(const GDBusPropertyTable *property, void *data)
    631 {
    632         struct media_transport *transport = data;
    633         struct a2dp_transport *a2dp = transport->data;
    634
    635         // return a2dp->volume <= 127;
    636         return TRUE; // force true so we can change AirPod volume
    637 }

Build and install:

    dpkg-buildpackage -rfakeroot -uc -b
    sudo dpkg -i ../bluez_5.50-1.2~deb10u1_amd64.deb

Reboot.

Next, use the ``list_airpods.sh`` and ``airvol.sh`` scripts from
[this post on the manjaro forums](https://archived.forum.manjaro.org/t/airpods-pro-max-volume-too-low/146508/5):

``list_airpods.sh``

    #!/bin/bash
    dbus-send --print-reply --system --dest=org.bluez / org.freedesktop.DBus.ObjectManager.GetManagedObjects | grep -E '/org/bluez/hci./dev_.._.._.._.._.._../fd[0-9]+' -o

``airvol.sh``

    #!/bin/bash
    echo Setting volume to $1
    for dev in $(./list_airpods.sh); do dbus-send --print-reply --system --dest=org.bluez "$dev" org.freedesktop.DBus.Properties.Set string:org.bluez.MediaTransport1 string:Volume variant:uint16:$1; done

Now we can adjust the AirPods volume!

    ✓ 07:20:45 carlo@x4 ~ $ ./airvol.sh 90
    Setting volume to 90
    method return time=1621509655.344706 sender=:1.7 -> destination=:1.82 serial=220 reply_serial=2

It would be nice if this was a slider in pavucontrol.
