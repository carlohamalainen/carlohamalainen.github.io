---
author: Carlo Hamalainen
date: "2010-06-18T00:00:00Z"
title: Recovery of data from a RAID 5 disk
url: /2010/06/18/recovery-of-data-from-a-raid-5-disk/
---
At work I replaced a single drive in a HP DL380 RAID 5 array. The drive was only giving SMART 1720 errors (imminent failure) and HP wanted the drive back due to warranty conditions, so the question came up of erasing any data on the drive.

I got conflicting advice -- some people said that you would definitely be able to read data off the drive, and other people said that because it was part of a RAID 5 array, it was impossible to reconstruct the array, so the drive could be sent back with no worries. I decided to test how much data could be read from a single drive from a three disk RAID 5 array.

Details of the server: HP DL380, Smart Array P400 controller. The RAID 5 array was configured with two logical volumes.

Details of the drive:

    72GB 2.5" Serial Attached SCSI (SAS) SFF
    Single Port Hot-Plug 15K HDD
    Option Part# 431935-B21
    Spare Part# 432321-001
    Assembly Part# 431930-002
    Model# DH072ABAA6

Due to physical access issues the drive was taken out of the server and installed in a standard desktop PC running Windows XP with a Promise FastTrak TX2650 SAS controller card. After installing the TX2650 drivers the SAS drive was recognised as a standard hard drive using JBOD, so it immediately appeared as a logical drive in Windows XP. Here's the card and drive (fortunately the TX2650 comes with all the cables that you need):

{{< figure src="/stuff/wp-content/uploads/2010/06/card_and_drive.png" >}}

I ran [PhotoRec](http://www.cgsecurity.org/wiki/PhotoRec) directly on the SAS drive:

{{< figure src="/stuff/wp-content/uploads/2010/06/photorec1.png" >}}

After about two hours PhotoRec finished:

{{< figure src="/stuff/wp-content/uploads/2010/06/photorec2.png" >}}

Those recovered files total about 8Gb (the original RAID 5 array contained about 50Gb of data). From our perspective, the best hit is searching for a certain prefix "PATNOK" that we use in files for daily demographics imports:

{{< figure src="/stuff/wp-content/uploads/2010/06/photorec-patnok.png" >}}

Each of those 1443 files contains _at least_ one set of patient details (name, address, Medicare number, date of birth, phone number, next of kin, next of kin contact details).

That's a clear example of sensitive data coming off a single drive from a RAID 5 array.

The success rate for larger files was pretty low, I suspect due to the fact that data is striped on a RAID 5 disk.

