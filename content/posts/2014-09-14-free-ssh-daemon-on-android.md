---
author: Carlo Hamalainen

date: "2014-09-14T00:00:00Z"
format: image
guid: http://carlo-hamalainen.net/2014/09/14/free-ssh-daemon-on-android/
title: Free SSH daemon on Android
url: /2014/09/14/free-ssh-daemon-on-android/
---
In the past I have used SSHDroid to ssh into my phone. But recently the app seems to have split into a free and paid-for version, and you [can't use ssh keys with the free version](http://forum.xda-developers.com/showthread.php?t=921801) (as far as I can tell). So I went looking for a simple way to run an ssh daemon. 

[Lil' Debian](https://github.com/guardianproject/lildebi) lets you run Debian in a chroot environment on Android. It automatically mounts the Android file system for you, and also runs sshd automatically. Installing it was completely straightforward (I did it via [F-Droid](https://f-droid.org/)). My [notes](https://github.com/carlohamalainen/dotfiles/blob/master/cyanogenmod.md) on installing CyanogenMod and F-Droid are below, but you could also install Lil' Debian via the normal Play Store.

<http://gist-it.appspot.com/github/carlohamalainen/dotfiles/blob/master/cyanogenmod.md> 

Here's what I see when logged in to my phone: 

```
root@localhost:~# uptime
 16:53:39 up  4:40,  1 user,  load average: 5.94, 11.37, 6.61

root@localhost:~# uname -a
Linux localhost 3.0.64-CM-gd6f2f4a #1 SMP PREEMPT Mon Dec 2 21:12:34 PST 2013 armv7l GNU/Linux

root@localhost:~# free -m
             total       used       free     shared    buffers     cached
Mem:           832        813         19          0          2        213
-/+ buffers/cache:        597        235
Swap:            0          0          0

root@localhost:~# df -h
df: `/mnt/fuse': No such file or directory
df: `/preload': No such file or directory
Filesystem             Size  Used Avail Use% Mounted on
rootfs                 592M  252M  310M  45% /
tmpfs                  592M  252M  310M  45% /dev
tmpfs                  592M  252M  310M  45% /mnt/secure
tmpfs                  417M     0  417M   0% /mnt/asec
tmpfs                  417M     0  417M   0% /mnt/obb
/dev/block/mmcblk0p3    20M  8.6M   12M  44% /efs
/dev/block/mmcblk0p9   1.5G  437M  1.1G  29% /system
/dev/block/mmcblk0p8  1008M   18M  991M   2% /cache
/dev/block/mmcblk0p12   12G  980M   11G   9% /data
/dev/fuse               12G  980M   11G   9% /mnt/shell/emulated
/dev/block/loop0       592M  252M  310M  45% /
tmpfs                  417M     0  417M   0% /tmp
/dev/fuse               12G  980M   11G   9% /mnt/sdcard
/dev/block/mmcblk0p8  1008M   18M  991M   2% /cache
/dev/block/mmcblk0p12   12G  980M   11G   9% /data
/dev/block/mmcblk0p3    20M  8.6M   12M  44% /efs
tmpfs                  417M     0  417M   0% /mnt/asec
tmpfs                  417M     0  417M   0% /mnt/obb
/dev/fuse               12G  980M   11G   9% /mnt/shell/emulated
/dev/block/mmcblk0p9   1.5G  437M  1.1G  29% /system

root@localhost:~# route -n
Kernel IP routing table
Destination     Gateway         Genmask         Flags Metric Ref    Use Iface
0.0.0.0         192.168.1.1     0.0.0.0         UG    0      0        0 wlan0
0.0.0.0         192.168.1.1     0.0.0.0         UG    308    0        0 wlan0
192.168.1.0     0.0.0.0         255.255.255.0   U     0      0        0 wlan0
192.168.1.0     0.0.0.0         255.255.255.0   U     308    0        0 wlan0
192.168.1.1     0.0.0.0         255.255.255.255 UH    0      0        0 wlan0

root@localhost:~# cat /proc/cpuinfo
Processor   : ARMv7 Processor rev 0 (v7l)
processor   : 0
BogoMIPS    : 1592.52

Features    : swp half thumb fastmult vfp edsp neon vfpv3 tls
CPU implementer : 0x41
CPU architecture: 7
CPU variant : 0x3
CPU part    : 0xc09
CPU revision    : 0

Chip revision   : 0011
Hardware    : SMDK4x12
Revision    : 000c
Serial      : 230fbf574df75916
```
