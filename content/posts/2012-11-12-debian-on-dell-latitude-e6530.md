---
author: Carlo Hamalainen

date: "2012-11-12T00:00:00Z"
format: image
guid: http://carlo-hamalainen.net/2012/11/12/debian-on-dell-latitude-e6530/
id: 772
original_post_id:
- "16"
restapi_import_id:
- 596a05ef0330b
title: Debian on Dell Latitude E6530
url: /2012/11/12/debian-on-dell-latitude-e6530/
---

**Update 2012-12-13:** my 3.6.9 kernel config is [here](/stuff/config-3.6.9-debian-on-dell-latitude-E6530).

Ubuntu is getting a bit [weird](http://help.ubuntu-it.org/12.04/desktop/power-hibernate.html) these days so I decided to go back to Debian on my work laptop, a Dell Latitude E6530 with the NVIDIA NVS 5200M graphics card. Full output of ``lspci -v`` is at the end of this post.

Notes:

  * The sound card is not recognised by a 2.6.x kernel.
  * The 3.x kernel in squeeze-backports had issues.
  * The standard 3.x kernel in testing would not boot (kernel panic).
  * I copied a working kernel config from an Ubuntu 12.04 install, and disabled the nouveau module completely to avoid any clashes with the proprietary NVIDIA driver. My kernel config is [here](/stuff/config-3.2.33-debian-on-dell-latitude-E6530).

The installation process:

1. Install the base Debian Squeeze system but don't install the graphical desktop environment.

2. Replace "squeeze" with "testing" in /etc/apt/sources.list, then do the usual apt-get update and apt-get dist-upgrade.

3. Install a 3.2.x kernel from source:

```
apt-get install gcc build-essential make

cd /usr/src

wget http://www.kernel.org/pub/linux/kernel/v3.0/linux-3.2.33.tar.bz2
tar jxf linux-3.2.33.tar.bz2
cd linux-3.2.33

wget https://carlo-hamalainen.net/stuff/config-3.2.33-debian-on-dell-latitude-E6530 -O .config
make oldconfig
make-kpkg --rootcmd fakeroot --config menuconfig --initrd --us --uc -j 9 kernel_image

dpkg -i ../linux-image-3.2.33*deb
```

4. Reboot, and then install the latest NVIDIA drivers (I used NVIDIA-Linux-x86_64-304.64.run).

5. Install a graphical desktop environment:

    ```
    apt-get install gnome-desktop-environment # or whatever you like, e.g. xmonad, KDE, etc.
    ```

6. Reboot.

Output of lspci -v:

```
00:00.0 Host bridge: Intel Corporation 3rd Gen Core processor DRAM Controller (rev 09)
	Subsystem: Dell Device 0535
	Flags: bus master, fast devsel, latency 0
	Capabilities:

00:01.0 PCI bridge: Intel Corporation Xeon E3-1200 v2/3rd Gen Core processor PCI Express Root Port (rev 09) (prog-if 00 [Normal decode])
	Flags: bus master, fast devsel, latency 0
	Bus: primary=00, secondary=01, subordinate=01, sec-latency=0
	I/O behind bridge: 0000e000-0000efff
	Memory behind bridge: f5000000-f60fffff
	Prefetchable memory behind bridge: 00000000e0000000-00000000f1ffffff
	Capabilities:
	Kernel driver in use: pcieport

00:14.0 USB controller: Intel Corporation 7 Series/C210 Series Chipset Family USB xHCI Host Controller (rev 04) (prog-if 30 [XHCI])
	Subsystem: Dell Device 0535
	Flags: bus master, medium devsel, latency 0, IRQ 43
	Memory at f7620000 (64-bit, non-prefetchable) [size=64K]
	Capabilities:
	Kernel driver in use: xhci_hcd

00:16.0 Communication controller: Intel Corporation 7 Series/C210 Series Chipset Family MEI Controller #1 (rev 04)
	Subsystem: Dell Device 0535
	Flags: bus master, fast devsel, latency 0, IRQ 45
	Memory at f763c000 (64-bit, non-prefetchable) [size=16]
	Capabilities:
	Kernel driver in use: mei

00:19.0 Ethernet controller: Intel Corporation 82579LM Gigabit Network Connection (rev 04)
	Subsystem: Dell Device 0535
	Flags: bus master, fast devsel, latency 0, IRQ 44
	Memory at f7600000 (32-bit, non-prefetchable) [size=128K]
	Memory at f7639000 (32-bit, non-prefetchable) [size=4K]
	I/O ports at f040 [size=32]
	Capabilities:
	Kernel driver in use: e1000e

00:1a.0 USB controller: Intel Corporation 7 Series/C210 Series Chipset Family USB Enhanced Host Controller #2 (rev 04) (prog-if 20 [EHCI])
	Subsystem: Dell Device 0535
	Flags: bus master, medium devsel, latency 0, IRQ 16
	Memory at f7638000 (32-bit, non-prefetchable) [size=1K]
	Capabilities:
	Kernel driver in use: ehci_hcd

00:1b.0 Audio device: Intel Corporation 7 Series/C210 Series Chipset Family High Definition Audio Controller (rev 04)
	Subsystem: Dell Device 0535
	Flags: bus master, fast devsel, latency 0, IRQ 46
	Memory at f7630000 (64-bit, non-prefetchable) [size=16K]
	Capabilities:
	Kernel driver in use: snd_hda_intel

00:1c.0 PCI bridge: Intel Corporation 7 Series/C210 Series Chipset Family PCI Express Root Port 1 (rev c4) (prog-if 00 [Normal decode])
	Flags: bus master, fast devsel, latency 0
	Bus: primary=00, secondary=02, subordinate=02, sec-latency=0
	Capabilities:
	Kernel driver in use: pcieport

00:1c.1 PCI bridge: Intel Corporation 7 Series/C210 Series Chipset Family PCI Express Root Port 2 (rev c4) (prog-if 00 [Normal decode])
	Flags: bus master, fast devsel, latency 0
	Bus: primary=00, secondary=03, subordinate=03, sec-latency=0
	Memory behind bridge: f7500000-f75fffff
	Capabilities:
	Kernel driver in use: pcieport

00:1c.2 PCI bridge: Intel Corporation 7 Series/C210 Series Chipset Family PCI Express Root Port 3 (rev c4) (prog-if 00 [Normal decode])
	Flags: bus master, fast devsel, latency 0
	Bus: primary=00, secondary=04, subordinate=07, sec-latency=0
	I/O behind bridge: 0000d000-0000dfff
	Memory behind bridge: f6b00000-f74fffff
	Prefetchable memory behind bridge: 00000000f2b00000-00000000f34fffff
	Capabilities:
	Kernel driver in use: pcieport

00:1c.3 PCI bridge: Intel Corporation 7 Series/C210 Series Chipset Family PCI Express Root Port 4 (rev c4) (prog-if 00 [Normal decode])
	Flags: bus master, fast devsel, latency 0
	Bus: primary=00, secondary=08, subordinate=0b, sec-latency=0
	I/O behind bridge: 0000c000-0000cfff
	Memory behind bridge: f6100000-f6afffff
	Prefetchable memory behind bridge: 00000000f2100000-00000000f2afffff
	Capabilities:
	Kernel driver in use: pcieport

00:1d.0 USB controller: Intel Corporation 7 Series/C210 Series Chipset Family USB Enhanced Host Controller #1 (rev 04) (prog-if 20 [EHCI])
	Subsystem: Dell Device 0535
	Flags: bus master, medium devsel, latency 0, IRQ 21
	Memory at f7637000 (32-bit, non-prefetchable) [size=1K]
	Capabilities:
	Kernel driver in use: ehci_hcd

00:1f.0 ISA bridge: Intel Corporation QM77 Express Chipset LPC Controller (rev 04)
	Subsystem: Dell Device 0535
	Flags: bus master, medium devsel, latency 0
	Capabilities:

00:1f.2 SATA controller: Intel Corporation 7 Series Chipset Family 6-port SATA Controller [AHCI mode] (rev 04) (prog-if 01 [AHCI 1.0])
	Subsystem: Dell Device 0535
	Flags: bus master, 66MHz, medium devsel, latency 0, IRQ 42
	I/O ports at f090 [size=8]
	I/O ports at f080 [size=4]
	I/O ports at f070 [size=8]
	I/O ports at f060 [size=4]
	I/O ports at f020 [size=32]
	Memory at f7636000 (32-bit, non-prefetchable) [size=2K]
	Capabilities:
	Kernel driver in use: ahci

00:1f.3 SMBus: Intel Corporation 7 Series/C210 Series Chipset Family SMBus Controller (rev 04)
	Subsystem: Dell Device 0535
	Flags: medium devsel, IRQ 18
	Memory at f7635000 (64-bit, non-prefetchable) [size=256]
	I/O ports at f000 [size=32]

01:00.0 VGA compatible controller: NVIDIA Corporation Device 0dfc (rev a1) (prog-if 00 [VGA controller])
	Subsystem: Dell Device 0535
	Flags: bus master, fast devsel, latency 0, IRQ 16
	Memory at f5000000 (32-bit, non-prefetchable) [size=16M]
	Memory at e0000000 (64-bit, prefetchable) [size=256M]
	Memory at f0000000 (64-bit, prefetchable) [size=32M]
	I/O ports at e000 [size=128]
	[virtual] Expansion ROM at f6000000 [disabled] [size=512K]
	Capabilities:
	Kernel driver in use: nvidia

01:00.1 Audio device: NVIDIA Corporation GF108 High Definition Audio Controller (rev a1)
	Subsystem: Dell Device 0535
	Flags: bus master, fast devsel, latency 0, IRQ 17
	Memory at f6080000 (32-bit, non-prefetchable) [size=16K]
	Capabilities:
	Kernel driver in use: snd_hda_intel

03:00.0 Network controller: Intel Corporation Centrino Ultimate-N 6300 (rev 35)
	Subsystem: Intel Corporation Centrino Ultimate-N 6300 3x3 AGN
	Flags: bus master, fast devsel, latency 0, IRQ 47
	Memory at f7500000 (64-bit, non-prefetchable) [size=8K]
	Capabilities:
	Kernel driver in use: iwlwifi
```

**Archived Comments**

Date: 2012-11-15 02:24:54 UTC

Author: JB

Hi, thanks for this useful webpage! A simple question though: why haven't you used more recent a kernel? Seems you wrote this page only two days ago, why not having chosen 3.6.6?

(I'm asking because I now have issues with this 3.6.6 kernel on the very same Latitude E6530... Might try with your 3.2.33, but now before I can understand why ;-))

Date: 2012-11-15 07:44:56 UTC

Author: KarolSzk

Hello,

I'm interested wheter suspend/resume for a few days works ok or not with nvidia/wheezy and your Dell E6530. I have a E6510 with ubuntu/nouveau (previously nvidia closed driver) and suspend/resume doesn't work properly..

Date: 2012-11-15 08:55:09 UTC

Author: carlo

JB: I did try 3.6.6 but the webcam wasn't detected. I need it for daily meetings and haven't had a chance to work out what's wrong, so for the moment I'm using 3.2.33.

KarolSzk: I will try this out over the next week and report back.

Date: 2012-11-15 21:21:37 UTC

Author: JB

Thanks for the quick answer! With the 3.6.6 kernel built with the original options it comes out with (so lots of included drivers that are of no use for my system, but it's ok for lazy people, at least I don't miss any that is actually useful), it wouldn't boot out of grub. With precisely the same settings as my working 2.6.32-5-686, same uuid for the hard disk, etc, 3.6.6 complains that no device is bound to boot from... Weired, isn't it? I'll probably go for a 3.2.33 with some care in the choice of the modules to use as builtin, when I have some spare time...

Date: 2012-11-16 20:03:46 UTC

Author: JB

Sorry, silly me! I had missed the make modules-install step before the make install. Now 3.6.6 runs fine, soundcard works nicely, for the moment I've not tried the camera.

With the NVidia proprietary driver version 304.64, it's a nice display also! 🙂

Date: 2012-11-20 18:23:16 UTC

Author: JB

Hi again, have you managed to get the key controls properly setting the brightness of the screen? In my case the gnome applet appears on the screen, but nothing happens concerning the brightness 🙁

Date: 2012-11-20 18:32:09 UTC

Author: carlo

JB: yes, brightness keys do work for me with 3.2.33 and the MATE desktop (or xmonad with gnome-session running).

Date: 2012-11-28 00:40:25 UTC

Author: JB

Ok, from this link ([https://bugs.launchpad.net/ubuntu/+source/linux/+bug/773710](https://bugs.launchpad.net/ubuntu/+source/linux/+bug/773710)) is learned that I had to add the line:

    Option "RegistryDwords" "EnableBrightnessControl=1"

to my /etc/X11/xorg.conf, in the section corresponding to Screen0 for this NVidia card. It works now! 🙂
