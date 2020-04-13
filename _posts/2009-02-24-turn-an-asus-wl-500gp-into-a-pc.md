---
id: 758
title: Turn an Asus WL-500gP into a PC
date: 2009-02-24T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2009/02/24/turn-an-asus-wl-500gp-into-a-pc/
permalink: /2009/02/24/turn-an-asus-wl-500gp-into-a-pc/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
Update (2009-04-23): after a reboot my WL-500gP no longer booted Debian. Nothing appeared in the logs, so it really had me stumped. I ended up installing DD-WRT and all is well.

Here are my notes for installing Debian on an Asus WL-500gP (Premium v1). I basically followed [this guide](http://wpkg.org/Running_Debian_on_ASUS_WL-500G_deluxe). I'm using the WL-500gP as a PC, not a router, so I have no information here about how to configure the VLAN stuff using robocfg.

**Flash the kernel image**

Install tftp on your PC, download the kernel openwrt-brcm47xx-2.6-jffs2-128k.trx and Debian image debian-mipsel-2007-Apr-17.tar.bz2 from [here](http://wpkg.org/Running_Debian_on_ASUS_WL-500G_deluxe:Downloads) (or try my [local copy](http://carlo-hamalainen.net/debian-asus-WL-500gP/) but be sure to check the md5sums).

Set your PC to static IP 192.168.1.3 with gateway 192.168.1.1

Turn off the router. Hold down the "restore" button on the back, and turn on the power. Hold the button until the power LED on the front of the router flashes slowly.

The router is now in diag mode. Upload the image:

    $ tftp 192.168.1.1

    tftp> binary

    tftp> trace

    Packet tracing on.

    tftp> put openwrt-brcm-2.6-jffs2-128k.trx

**Prepare your USB disk on your PC**

I assume that your USB disk is ``/dev/sda`` but it's likely to be something else, so adjust accordingly. Partition your USB disk so that ``/dev/sda1`` is ext3, ``/dev/sda2`` is swap. It is **essential** to make sure that the USB drive is not checked by e2fsck:

    $ tune2fs -c0 -i0 /dev/sda1

Uncompress ``debian-mipsel-2007-Apr-17.tar.bz2`` in the ``/dev/sda1`` partition so that ``/bin``, ``/etc`` are at the top level.

Run ``mkswap`` on ``/dev/sda2``

Configure the network.

My home network (via ADSL modem) is 192.168.1.x so my ``/etc/network/interfaces`` is:

    auto lo
    iface lo inet loopback

    #auto eth0
    #iface eth0 inet dhcp

    auto eth0.100
    iface eth0.100 inet dhcp

    auto eth0
    iface eth0 inet static
    address 192.168.1.66
    netmask 255.255.255.0
    gateway 192.168.1.1

    #auto eth0.100
    #iface eth0.100 inet static
    #address 192.168.1.25
    #netmask 255.255.255.0
    #network 192.168.1.128
    #broadcast 192.168.1.191
    #gateway 192.168.1.1

    # wireless doesn't work yet
    #auto eth1
    #iface eth1 inet static
    #   address 192.168.5.1
    #   netmask 255.255.255.0
    #   gateway 192.168.x.x
    #   wireless-essid networkname
    #   wireless-key 00000000000000000

My home router provides DNS so my ``/etc/resolv.conf`` looks like:

    nameserver 192.168.1.1

``/etc/hostname``:

    asusbox

Unmount the USB drive, plug it into the Asus.

For ethernet, I found that the LAN1 port worked (I thought that the WAN port would but it didn't). Connect the Asus to your main router via the LAN1 port as if it was a normal PC.

Power it up. It should respond to ping,and ssh. The default root password is toor, and it should be changed as soon as possible.

I have an old laptop hard drive attached to my WL-500gP using an IDE to USB cable:

<img src="/s3/oldblog/blogdata/medium/2009-02-21++19-55-48.jpg" border="0" alt="[photo]" data-recalc-dims="1" />

Nice little silent PC:

<img src="/s3/oldblog/blogdata/medium/2009-02-21++19-56-26.jpg" border="0" alt="[photo]" data-recalc-dims="1" />

Some system info:

    carlo@lothlorien:~$ cat /proc/cpuinfo
    system type             : Broadcom BCM47xx
    processor               : 0
    cpu model               : Broadcom BCM3302 V0.6
    BogoMIPS                : 263.16
    wait instruction        : no
    microsecond timers      : yes
    tlb_entries             : 32
    extra interrupt vector  : no
    hardware watchpoint     : no
    ASEs implemented        :
    VCED exceptions         : not available
    VCEI exceptions         : not available

    carlo@lothlorien:~$ free -m
    total       used       free     shared    buffers     cached
    Mem:            29         28          1          0          0         16
    -/+ buffers/cache:         11         18
    Swap:          729          5        724
    carlo@lothlorien:~$ df -h
    Filesystem            Size  Used Avail Use% Mounted on
    /dev/sda1             147G  103G   37G  74% /
    tmpfs                  15M     0   15M   0% /lib/init/rw
    udev                   10M   20K   10M   1% /dev
    tmpfs                  15M  4.0K   15M   1% /dev/shm
    none                   15M     0   15M   0% /tmp

    lothlorien:~# lspci
    00:00.0 Host bridge: Broadcom Corporation BCM4704 PCI to SB Bridge (rev 09)
    00:02.0 Network controller: Broadcom Corporation BCM4318 [AirForce One 54g] 802.11g Wireless LAN Controller (rev 02)
    00:03.0 USB Controller: VIA Technologies, Inc. VT82xxxxx UHCI USB 1.1 Controller (rev 62)
    00:03.1 USB Controller: VIA Technologies, Inc. VT82xxxxx UHCI USB 1.1 Controller (rev 62)
    00:03.2 USB Controller: VIA Technologies, Inc. USB 2.0 (rev 65)

It can handle rtorrent but uses a few Mb of swap and so it's not too quick to respond sometimes in an ssh terminal.

**Archived Comments**

Date: 2009-02-25 19:11:49 UTC

Author: Nadiah

Damn that's sexy 😉

Date: 2012-05-23 09:51:26 UTC

Author: A

Yes, very cool.
