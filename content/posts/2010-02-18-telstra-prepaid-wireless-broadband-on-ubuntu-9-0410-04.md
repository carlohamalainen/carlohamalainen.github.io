---
author: Carlo Hamalainen

date: "2010-02-18T00:00:00Z"
format: image
guid: http://carlo-hamalainen.net/2010/02/18/telstra-prepaid-wireless-broadband-on-ubuntu-9-0410-04/
title: Telstra prepaid wireless broadband on Ubuntu 9.04/10.04
url: /2010/02/18/telstra-prepaid-wireless-broadband-on-ubuntu-9-0410-04/
---
**Update 2011-03-31:** the MF626 is complete crap. Constant dropouts. Switched to [Internode 3G](http://www.internode.on.net/residential/wireless_broadband/nodemobile_data/) with their Huawei hardware. Much better.

I recently bought a Telstra NextG prepaid wireless broadband pack and found some of the information for Ubuntu 9.04 on forums/blogs to be a bit out of date. Here's my quick start guide:

**Update for Ubuntu 10.10 64bit:** the modem is not recognised so for a quick fix I just installed the usb-modeswitch and usb-modeswitch-data packages from Ubuntu Lucid.

**Update for Ubuntu 10.04:** just install the usb-modeswitch package and the data package (see [this page](https://bugs.launchpad.net/ubuntu/+source/linux/+bug/546728) for the bug report and workaround):

    sudo aptitude install usb-modeswitch usb-modeswitch-data

That's it. For Ubuntu 9.04, read on:

If you plug in the MF626 modem it will default to USB storage mode and no modem will be detected. (This is to make installing drivers easy for Windows and OSX users.) On Ubuntu 9.04 the ``usb_modeswitch`` package is not available by default so one must compile and install it manually.

First make sure that you have the USB deveopment library:

    sudo apt-get update
    sudo apt-get install libusb-dev

Go to the [usb_modeswitch download page](http://www.draisberghof.de/usb_modeswitch/#download) and get usb-modeswitch-1.1.0.tar.bz2 (or later).

    tar jxf usb-modeswitch-1.1.0.tar.bz2
    cd usb-modeswitch-1.1.0
    make
    sudo make install

With version 1.1.0 or greater you do not need to edit the usb-modeswitch configuration files as some guides suggest.

Restart. Disable wireless connections (right click the connection manager). Plug in the USB wireless modem, wait a bit, and a new connection wizard should pop up.

Configure the modem as Telstra -- UMTS / HSDPA.

You should now be able to connect. It doesn't ask for a password because all authentication is done via the SIM card.

If this doesn't work, check the output of **lsusb** and make sure that a device appears with ID 19d2:0031. If you see a device with ID 19d2:2000 then the ``usb_modeswitch`` program has failed.

Todo: check balance and recharge: [Whirlpool forum notes](http://forums.whirlpool.net.au/forum-replies.cfm?t=1072789&r=17344691#r17344691) .
