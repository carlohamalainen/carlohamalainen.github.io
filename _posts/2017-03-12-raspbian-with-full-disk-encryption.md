---
id: 762
title: Raspbian with full disk encryption
date: 2017-03-12T00:00:00+00:00
author: Carlo Hamalainen
layout: post
guid: http://carlo-hamalainen.net/2017/03/12/raspbian-with-full-disk-encryption/
permalink: /2017/03/12/raspbian-with-full-disk-encryption/
restapi_import_id:
  - 596a05ef0330b
original_post_id:
  - "16"
categories:
  - Uncategorized
format: image
---
This blog post shows how to convert a standard [Raspbian](https://www.raspberrypi.org/downloads/raspbian/) installation to full disk encryption. The encryption passphrase can be entered at the physical console or via a dropbear ssh session. 

I mainly follow the [Offensive Security guide](https://www.offensive-security.com/kali-linux/raspberry-pi-luks-disk-encryption/). 

What you need: 

  * Raspberry Pi. 
  * Laptop with a microSD card slot. I used my X1 Carbon running Ubuntu xenial (amd64). 

First, [install Raspbian](https://www.raspberrypi.org/documentation/installation/installing-images/README.md). With a 32Gb microSD card the partitions are: 

```
/dev/mmcblk0p2                29G  4.8G   23G  18% /media/carlo/7f593562-9f68-4bb9-a7c9-2b70ad620873
/dev/mmcblk0p1                63M   21M   42M  34% /media/carlo/boot
```

It's a good idea to make a backup of the working installation: 

```
dd if=/dev/mmcblk0 of=pi-debian-unencrypted-backup.img
```

Also make a note of the start/end of the main partition. This will be needed later. 

Install the qemu static (on the laptop, not the Pi): 

```
sudo apt update
sudo apt install qemu-user-static
```

Create directories for the chroot. Easiest to do all of this as root. Pop the sd card into the laptop and drop into a chroot: 

```
mkdir -p pi/chroot/boot

mount /dev/mmcblk0p2 pi/chroot/
mount /dev/mmcblk0p1 pi/chroot/boot/

mount -t proc  none     pi/chroot/proc
mount -t sysfs none     pi/chroot/sys
mount -o bind /dev      pi/chroot/dev
mount -o bind /dev/pts  pi/chroot/dev/pts

cp /usr/bin/qemu-arm-static pi/chroot/usr/bin/
LANG=C chroot pi/chroot/
```

Next we need to install a few things in the chroot. If these fail with 

```
root@x4:/# apt update
qemu: uncaught target signal 4 (Illegal instruction) - core dumped
Illegal instruction (core dumped)
```

then comment out the libarmmem line in ld.so.preload: 

```
root@x4:~# cat pi/chroot/etc/ld.so.preload
#/usr/lib/arm-linux-gnueabihf/libarmmem.so
```

Install the things: 

```
apt update
apt install busybox cryptsetup dropbear
```

To create an initramfs image we we need the kernel version. In my case  
it is 4.4.50-v7+. 

```
root@x4:/# ls -l /lib/modules/
total 8
drwxr-xr-x 3 root root 4096 Mar 11 20:31 4.4.50+
drwxr-xr-x 3 root root 4096 Mar 11 20:31 4.4.50-v7+
```

Create the image, enable ssh, and set the root password: 

```
root@x4:/# mkinitramfs -o /boot/initramfs.gz 4.4.50-v7+
root@x4:/# update-rc.d ssh enable
root@x4:/# passwd
```

Set the boot command line. Previously I had: 

```
root@x4:/# cat /boot/cmdline.txt
dwc_otg.lpm_enable=0 console=serial0,115200 console=tty1 root=/dev/mmcblk0p2 rootfstype=ext4 elevator=deadline fsck.repair=yes rootwait
```

The new one refers to the encrypted partition: 

```
dwc_otg.lpm_enable=0 console=serial0,115200 console=tty1 root=/dev/mapper/crypt_sdcard cryptdevice=/dev/mmcblk0p2:crypt_sdcard rootfstype=ext4 elevator=deadline fsck.repair=yes rootwait
```

Add this to the boot config: 

```
echo "initramfs initramfs.gz 0x00f00000" >> /boot/config.txt
```

Cat the private key and copy to the laptop; save as pikey: 

```
root@x4:/# cat /etc/initramfs-tools/root/.ssh/id_rsa
```

The Offensive Security guide talks about editing /etc/initramfs-tools/root/.ssh/authorized_keys so that the ssh login can only run /scripts/local-top/cryptroot. I had no luck getting this to work, running into weird issues with Plymouth. For some reason the password prompt appeared on the physical console of the Pi, not the ssh session. So I skipped this and manually use the dropbear session to enter the encryption passphrase. 

Set /etc/fstab to point to the new root partition. Original file: 

```
root@x4:/# cat /etc/fstab
proc            /proc           proc    defaults          0       0
/dev/mmcblk0p1  /boot           vfat    defaults          0       2
/dev/mmcblk0p2  /               ext4    defaults,noatime  0       1
# a swapfile is not a swap partition, no line here
#   use  dphys-swapfile swap[on|off]  for that
```

New file (only one line changed, referring to /dev/mapper): 

```
root@x4:/# cat /etc/fstab
proc            /proc           proc    defaults          0       0
/dev/mmcblk0p1  /boot           vfat    defaults          0       2
/dev/mapper/crypt_sdcard /               ext4    defaults,noatime  0       1
# a swapfile is not a swap partition, no line here
#   use  dphys-swapfile swap[on|off]  for that
```

Edit /etc/crypttab to look like this: 

```
root@x4:/# cat /etc/crypttab
# 				
crypt_sdcard /dev/mmcblk0p2 none luks
```

The Offensive Security guide mentions that there can be issues with ports taking a while to wake up, so they recommend adding a 5 second sleep before the configure_networking line in /usr/share/initramfs-tools/scripts/init-premount/dropbear: 

```
echo "Waiting 5 seconds for USB to wake"
sleep 5
configure_networking &
```

Regenerate the image: 

```
root@x4:/# mkinitramfs -o /boot/initramfs.gz 4.4.50-v7+
device-mapper: table ioctl on crypt_sdcard failed: No such device or address
Command failed
cryptsetup: WARNING: failed to determine cipher modules to load for crypt_sdcard
Unsupported ioctl: cmd=0x5331
```

Now pop out of the chroot (Ctrl-D), unmount some things, and make a backup: 

```
umount pi/chroot/boot
umount pi/chroot/sys
umount pi/chroot/proc
mkdir -p pi/backup
rsync -avh pi/chroot/* pi/backup/

umount pi/chroot/dev/pts
umount pi/chroot/dev
umount pi/chroot
```

Encrypt the partition, unlock it, and rsync the data back: 

```
cryptsetup -v -y --cipher aes-cbc-essiv:sha256 --key-size 256 luksFormat /dev/mmcblk0p2
cryptsetup -v luksOpen /dev/mmcblk0p2 crypt_sdcard
mkfs.ext4 /dev/mapper/crypt_sdcard

mkdir -p pi/encrypted
mount /dev/mapper/crypt_sdcard pi/encrypted/
rsync -avh pi/backup/* pi/encrypted/

umount pi/encrypted/
cryptsetup luksClose /dev/mapper/crypt_sdcard
sync
```

Now put the sd card into the Pi and boot it up. If you see this on the console: 

```
/scripts/local-top/cryptroot: line 1: /sbin/cryptsetup: not found
```

it means that the initramfs image didn't include the cryptsetup binary. It is a [known bug](https://bugs.launchpad.net/ubuntu/+source/cryptsetup/+bug/1256730/comments/4) and the workaround that worked for me was: 

```
echo "export CRYPTSETUP=y" >> /usr/share/initramfs-tools/conf-hooks.d/forcecryptsetup
```

(I had to do this in the chroot environment and rebuild the initramfs image. Ugh.) 

For some reason I had Plymouth asking for the password on the physical console instead of the ssh dropbear connection. This is another [known issue](https://bugs.launchpad.net/ubuntu/+source/cryptsetup/+bug/595648). This [workaround](https://bugs.launchpad.net/ubuntu/+source/cryptsetup/+bug/595648/comments/5) looked promising but it broke the physical console as well as the ssh connection. No idea why. 

What does work for me is to use the dropbear session to [manually kill Plymouth and then enter the encryption password](https://bugs.launchpad.net/ubuntu/+source/cryptsetup/+bug/595648/comments/19):

```
ssh -i pikey root@192.168.0.xxx
```

Then in the busybox session: 

```
kill $(pidof plymouthd)
# Wait a few seconds...
echo -ne password > /lib/cryptsetup/passfifo
/scripts/local-top/cryptroot
```

This lets you enter the encryption passphrase. After a few seconds the normal boot process continues. So you can enter the encryption passphrase with a real keyboard if you are physically with the Pi, or you can ssh in if you are remote.
